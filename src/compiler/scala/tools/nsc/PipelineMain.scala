/* NSC -- new Scala compiler
 * Copyright 2005-2018 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.util.Collections

import javax.tools.{SimpleJavaFileObject, ToolProvider}

import scala.collection.JavaConverters.asJavaIterableConverter
import scala.collection.{immutable, mutable, parallel}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.reflect.internal.pickling.PickleBuffer
import scala.reflect.internal.util.FakePos
import scala.reflect.io.{VirtualDirectory, VirtualFile}
import scala.tools.nsc.backend.{ClassfileInfo, JavaPlatform, ScalaClass, ScalaRawClass}
import scala.tools.nsc.classpath.{DirectoryClassPath, VirtualDirectoryClassPath, ZipArchiveFileLookup}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.ClassPath
import scala.util.{Failure, Success}
import scala.concurrent.duration.Duration
import scala.tools.nsc.classpath.ZipAndJarClassPathFactory.ZipArchiveClassPath

class PipelineMainClass(label: String, parallelism: Int, strategy: BuildStrategy) {
  /** Forward errors to the (current) reporter. */
  protected def scalacError(msg: String): Unit = {
    reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  private var reporter: Reporter = _

  implicit val executor = ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(parallelism))
  val fileManager = ToolProvider.getSystemJavaCompiler.getStandardFileManager(null, null, null)

  private class PickleClassPath[G <: Global](data: mutable.AnyRefMap[G#Symbol, PickleBuffer]) {
    val dir = new VirtualDirectory("fakes", None)
    val classpath = VirtualDirectoryClassPath(dir)
    val dirs = mutable.Map[G#Symbol, AbstractFile]()
    val classInfo = mutable.Map[AbstractFile, ClassfileInfo]()
    def packageDir(packSymbol: G#Symbol): AbstractFile = {
      if (packSymbol.isEmptyPackageClass) dir
      else if (dirs.contains(packSymbol)) dirs(packSymbol)
      else if (packSymbol.owner.isRoot) {
        val subDir = dir.subdirectoryNamed(packSymbol.encodedName)
        dirs.put(packSymbol, subDir)
        subDir
      } else {
        val base = packageDir(packSymbol.owner)
        val subDir = base.subdirectoryNamed(packSymbol.encodedName)
        dirs.put(packSymbol, subDir)
        subDir
      }
    }
    for ((symbol, pickle) <- data) {
      val base = packageDir(symbol.owner)
      if (symbol.isClass) {
        val primary = base.fileNamed(symbol.encodedName + ".class")
        classInfo(primary) = ScalaClass(symbol.fullNameString, () => ByteBuffer.wrap(pickle.bytes))
        if (symbol.companionModule.exists) {
          val secondary = base.fileNamed(symbol.companionModule.encodedName + "$.class")
          classInfo(secondary) = ScalaRawClass(symbol.companionModule.fullNameString)
        }
      } else if (symbol.isModule) {
          val primary = base.fileNamed(symbol.encodedName + ".class")
          classInfo(primary) = ScalaClass(symbol.fullNameString, () => ByteBuffer.wrap(pickle.bytes))
          val secondary = base.fileNamed(symbol.companionModule.encodedName + "$.class")
          classInfo(secondary) = ScalaRawClass(symbol.companionModule.fullNameString)
      }
    }
  }
  private val allPickleData =  new java.util.concurrent.ConcurrentHashMap[Path, PickleClassPath[_]]
  private val allParsedInfos = new java.util.concurrent.ConcurrentHashMap[AbstractFile, ClassfileInfo]

  def process(args: Array[String]): Boolean = {
    println(s"parallelism = $parallelism, strategy = $strategy")

    reporter = new ConsoleReporter(new Settings(scalacError))

    def commandFor(argFileArg: String): Task = {
      val ss   = new Settings(scalacError)
      val command = new CompilerCommand(("@" + argFileArg) :: Nil, ss)
      Task(argFileArg, command, command.files)
    }
    val projects: List[Task] = args.toList.map(commandFor)
    val produces = mutable.HashMap[Path, Task]()
    for (p <- projects) {
      val outputDir = p.command.settings.outputDirs.getSingleOutput.get.file.toPath.toAbsolutePath.normalize()
      produces(outputDir) = p
    }
    val dependsOn = mutable.HashMap[Task, List[Task]]()
    for (p <- projects) {
      val value: Seq[String] = ClassPath.expandPath(p.command.settings.classpath.value, expandStar = true)
      dependsOn(p) = value.flatMap(s => produces.get(Paths.get(s).toAbsolutePath.normalize())).toList.filterNot(_ == p)
    }
    val dependedOn: Set[Task] = dependsOn.valuesIterator.flatten.toSet
    val timer = new Timer
    timer.start()
    strategy match {
      case OutlineTypeOnly =>
        val futures = projects.map { p =>
          val f1 = Future.sequence[Unit, List](dependsOn.getOrElse(p, Nil).map(_.outlineDone.future))
          p.shouldOutlineType = true
          f1.map { _ =>
            p.outlineCompile()
            p.javaCompile()
          }
        }

        val toWait: Future[List[Unit]] = Future.sequence(futures).flatMap(_ => Future.sequence(projects.flatMap(p => p.javaDone.future :: p.outlineDone.future :: Nil) ))
        Await.result(toWait, Duration.Inf)
        timer.stop()

        for (p <- projects) {
          val dependencies = dependsOn(p)
          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max
          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }

        if (parallelism == 1) {
          val criticalPath = projects.maxBy(_.regularCriticalPathMs)
          println(f"Critical path: ${criticalPath.regularCriticalPathMs}%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else
          println(f" Wall Clock: ${timer.durationMs}%.0f ms")
      case OutlineTypePipeline =>
        val futures = projects.map { p =>
          val f1 = Future.sequence[Unit, List](dependsOn.getOrElse(p, Nil).map(_.outlineDone.future))
          val shouldOutlineType = dependedOn(p)
          p.shouldOutlineType = shouldOutlineType
          f1.map { _ =>
            if (p.shouldOutlineType) {
              p.outlineCompile()
            } else {
              p.fullCompile()
            }
          }
        }
        projects.map {
          p =>
            if (p.shouldOutlineType) p.outlineDone.future.onComplete { _ =>
              p.fullCompile()
            }
            Future.sequence(p.groups.map(_.done.future)).map(_ => p.javaCompile())
        }
        val toWait: Future[List[Unit]] = Future.sequence(futures).flatMap(_ => Future.sequence(projects.flatMap(p => p.javaDone.future :: p.groups.map(_.done.future) )))
        Await.result(toWait, Duration.Inf)
        timer.stop()

        for (p <- projects) {
          val dependencies = dependsOn(p)
          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max
          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }

        if (parallelism == 1) {
          val criticalPath = projects.maxBy(_.regularCriticalPathMs)
          println(f"Critical path: ${criticalPath.regularCriticalPathMs}%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else
          println(f" Wall Clock: ${timer.durationMs}%.0f ms")
      case Pipeline =>
        val futures: immutable.Seq[Future[Unit]] = projects.map { p =>
          val f1 = Future.sequence[Unit, List](dependsOn.getOrElse(p, Nil).map(_.outlineDone.future))
          val scalaCompiles: Future[Unit] = f1.map { _ => p.fullCompileExportPickles() }
          // Start javac after scalac has completely finished
          val f2 = Future.sequence[Unit, List](p.groups.map(_.done.future))
          val javaCompiles: Future[Unit] = f2.map { _ => p.javaCompile() }
          scalaCompiles.flatMap(_ => javaCompiles)
        }
        val toWait: Future[List[Unit]] = Future.sequence(futures).flatMap(_ => Future.sequence(projects.flatMap(p => p.javaDone.future :: p.groups.map(_.done.future) )))
        Await.result(toWait, Duration.Inf)
        timer.stop()

        for (p <- projects) {
          val dependencies = dependsOn(p)
          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max
          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }

        if (parallelism == 1) {
          val criticalPath = projects.maxBy(_.regularCriticalPathMs)
          println(f"Critical path: ${criticalPath.regularCriticalPathMs}%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else
          println(f" Wall Clock: ${timer.durationMs}%.0f ms")
      case Traditional =>
        val futures = projects.map { p =>
          val f1 = Future.sequence[Unit, List](dependsOn.getOrElse(p, Nil).map(_.javaDone.future))
          val shouldOutlineType = dependedOn(p)
          p.shouldOutlineType = shouldOutlineType
          f1.flatMap { _ =>
            p.fullCompile()
            Future.sequence(p.groups.map(_.done.future)).map(_ => p.javaCompile())
          }
        }
        val toWait: Future[List[Unit]] = Future.sequence(futures).flatMap(_ => Future.sequence(projects.flatMap(p => p.javaDone.future :: p.groups.map(_.done.future) )))
        Await.result(toWait, Duration.Inf)
        timer.stop()

        for (p <- projects) {
          val dependencies = dependsOn(p)
          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }
        if (parallelism == 1) {
          val maxFullCriticalPath: Double = projects.map(_.fullCriticalPathMs).max
          println(f"Critical path: $maxFullCriticalPath%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else {
          println(f"Wall Clock: ${timer.durationMs}%.0f ms")
        }
    }

    val trace = new java.lang.StringBuilder()
    trace.append("""{"traceEvents": [""")
    val sb = new mutable.StringBuilder(trace)
    def durationEvent(name: String, cat: String, t: Timer): String = {
      s"""{"name": "$name", "cat": "$cat", "ph": "X", "ts": ${(t.startMicros).toLong}, "dur": ${(t.durationMicros).toLong}, "pid": 0, "tid": ${t.thread.getId}}"""
    }
    def projectEvents(p: Task): List[String] = {
      val events = List.newBuilder[String]
      if (p.outlineTimer.durationMicros > 0d) {
        val desc = if (p.shouldOutlineType) "outline-type" else "parser-to-pickler"
        events += durationEvent(p.label, desc, p.outlineTimer)
      }
      for ((g, ix) <- p.groups.zipWithIndex) {
        if (g.timer.durationMicros > 0d)
          events += durationEvent(p.label, "compile-" + ix, g.timer)
      }
      if (p.javaTimer.durationMicros > 0d) {
        val desc = "javac"
        events += durationEvent(p.label, desc, p.javaTimer)
      }
      events.result()
    }
    projects.iterator.flatMap(projectEvents).addString(sb, ",\n")
    trace.append("]}")
    Files.write(Paths.get(s"build-${label}.trace"), trace.toString.getBytes())
    true
  }

  case class Group(files: List[String]) {
    val timer = new Timer
    val done = Promise[Unit]()
  }
  private case class Task(argsFile: String, command: CompilerCommand, files: List[String]) {
    val label = argsFile.replaceAll("target/", "").replaceAll("""(.*)/(.*).args""", "$1:$2")
    override def toString: String = argsFile

    command.settings.YcacheMacroClassLoader.value = "none"

    val groups: List[Group] = {
      val isScalaLibrary = files.exists(_.endsWith("Predef.scala"))
      if (strategy != OutlineTypePipeline || isScalaLibrary) {
        Group(files) :: Nil
      } else {
        command.settings.classpath.value = command.settings.outputDirs.getSingleOutput.get.toString + File.pathSeparator + command.settings.classpath.value
        val length = files.length
        val groups = (length.toDouble / 128).toInt.max(1)
        files.grouped((length.toDouble / groups).ceil.toInt.max(1)).toList.map(Group(_))
      }
    }
    command.settings.outputDirs.getSingleOutput.get.file.mkdirs()

    val isGrouped = groups.size > 1

    val outlineTimer = new Timer()
    val javaTimer = new Timer()

    var shouldOutlineType = true
    var outlineCriticalPathMs = 0d
    var regularCriticalPathMs = 0d
    var fullCriticalPathMs = 0d
    val outlineDone = Promise[Unit]()
    val javaDone = Promise[Unit]()

    lazy val compiler: Global = {
      val result = newCompiler(command.settings)
      val reporter = result.reporter
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporter.echo(command.getInfoMessage(compiler))
      result
    }

    def outlineCompile(): Unit = {
      outlineTimer.start()
      command.settings.Youtline.value = true
      command.settings.stopAfter.value = List("pickler")
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.None
      val run1 = new compiler.Run()
      run1 compile files
      allPickleData.put(command.settings.outputDirs.getSingleOutput.get.file.toPath.toRealPath().normalize(), new PickleClassPath(run1.symData))
      outlineTimer.stop()
      reporter.finish()
      if (reporter.hasErrors)
        outlineDone.complete(Failure(new RuntimeException("compile failed")))
      else
        outlineDone.complete(Success(()))
    }

    def fullCompile(): Unit = {
      command.settings.Youtline.value = false
      command.settings.stopAfter.value = Nil
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.Normal

      for (group <- groups) {
        group.done.completeWith {
          Future {
            val compiler2 = newCompiler(command.settings)
            val run2 = new compiler2.Run()
            group.timer.start()
            run2 compile group.files
            compiler2.reporter.finish()
            group.timer.stop()
            if (compiler2.reporter.hasErrors) {
              group.done.complete(Failure(new RuntimeException("Compile failed")))
            } else {
              group.done.complete(Success(()))
            }
          }
        }
      }
    }

    def fullCompileExportPickles(): Unit = {
      assert(groups.size == 1)
      val group = groups.head
      log("scalac: start")
      outlineTimer.start()
      val run2 = new compiler.Run() {
        override def advancePhase(): Unit = {
          if (compiler.phase == this.picklerPhase) {
            allPickleData.put(command.settings.outputDirs.getSingleOutput.get.file.toPath.toRealPath().normalize(), new PickleClassPath(symData))
            outlineTimer.stop()
            outlineDone.complete(Success(()))
            group.timer.start()
            log("scalac: exported pickles")
          }
          super.advancePhase()
        }
      }

      run2 compile group.files
      compiler.reporter.finish()
      group.timer.stop()
      log("scalac: done")
      if (compiler.reporter.hasErrors) {
        group.done.complete(Failure(new RuntimeException("Compile failed")))
      } else {
        group.done.complete(Success(()))
      }
    }

    def javaCompile(): Unit = {
      log("javac: start")
      val javaSources = files.filter(_.endsWith(".java"))
      if (javaSources.nonEmpty) {
        javaTimer.start()
        javaDone.completeWith(Future {
          val opts = java.util.Arrays.asList("-d", command.settings.outdir.value, "-cp", command.settings.outdir.value + File.pathSeparator + command.settings.classpath.value)
          val compileTask = ToolProvider.getSystemJavaCompiler.getTask(null, null, null, opts, null, fileManager.getJavaFileObjects(javaSources.toArray: _*))
          compileTask.setProcessors(Collections.emptyList())
          compileTask.call()
          javaTimer.stop()
          ()
        })
      } else {
        javaDone.complete(Success(()))
      }
      log("javac: start")
    }
    def log(msg: String) = () //Predef.println(this.label + ": " + msg)
  }

  final class Timer() {
    private var startNanos: Long = 0
    private var endNanos: Long = 0
    def start(): Unit = {
      assert(startNanos == 0L)
      startNanos = System.nanoTime
    }
    var thread: Thread = Thread.currentThread()
    def stop(): Unit = {
      thread = Thread.currentThread()
      endNanos = System.nanoTime()
    }
    def startMs: Double = startNanos.toDouble / 1000 / 1000
    def durationMs: Double = (endNanos - startNanos).toDouble / 1000 / 1000
    def startMicros: Double = startNanos.toDouble / 1000d
    def durationMicros: Double = (endNanos - startNanos).toDouble / 1000d
  }

  protected def newCompiler(settings: Settings): Global = {
    val g = Global(settings)

    val plugin: g.platform.ClassPathPlugin = new g.platform.ClassPathPlugin {
      val replacements = mutable.Buffer[PickleClassPath[_]]()
      override def modifyClassPath(classPath: Seq[ClassPath]): Seq[ClassPath] = {
        classPath.flatMap {
          case zcp: ZipArchiveFileLookup[_] =>
            val path = zcp.zipFile.toPath.toRealPath().normalize()
            allPickleData.get(path) match {
              case null =>
                zcp :: Nil
              case pcp =>
                replacements += pcp
                pcp.classpath :: Nil
            }
          case dcp: DirectoryClassPath =>
            val path = dcp.dir.toPath.toRealPath().normalize()
            allPickleData.get(path) match {
              case null =>
                dcp :: Nil
              case pcp =>
                replacements += pcp
                pcp.classpath :: Nil
            }
          case cp => cp :: Nil
        }
      }

      override def info(file: AbstractFile, clazz: g.ClassSymbol): Option[ClassfileInfo] = {
        file match {
          case vf: VirtualFile if vf.getClass == classOf[VirtualFile] =>
            val iterator = replacements.iterator.flatMap(_.classInfo.get(vf))
            if (iterator.hasNext)
              return Some(iterator.next())
            else None
          case _ => None
        }
        allParsedInfos.get(file) match {
          case null => None
          case info => Some(info)
        }
      }
      override def parsed(file: AbstractFile, clazz: g.ClassSymbol, info: ClassfileInfo): Unit = {
        allParsedInfos.put(file, info)
      }
    }
    g.platform.addClassPathPlugin(plugin)
    g
  }

}

sealed abstract class BuildStrategy
case object OutlineTypeOnly extends BuildStrategy
/** Outline type check to compute type signatures as pickles as an input to downstream compilation. */
case object OutlineTypePipeline extends BuildStrategy
case object Pipeline extends BuildStrategy
/** Emit class files before triggering downstream compilation */
case object Traditional extends BuildStrategy

object PipelineMain {
  def main(args: Array[String]): Unit = {
    val main = new PipelineMainClass("1", parallel.availableProcessors, Pipeline)
    val result = main.process(args)
    if (!result)
      System.exit(1)
    else
      System.exit(0)
  }
}

object PipelineMainTest {
  def main(args: Array[String]): Unit = {
    var i = 0
    for (_ <- 1 to 10; n <- List(parallel.availableProcessors); strat <- List(Pipeline, OutlineTypePipeline, Traditional)) {
        i += 1
        val main = new PipelineMainClass(i.toString, n, strat)
        println(s"====== ITERATION $i=======")
        val result = main.process(args)
        if (!result)
          System.exit(1)
    }
    System.exit(0)
  }
}
