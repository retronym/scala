/* NSC -- new Scala compiler
 * Copyright 2005-2018 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc

import java.io.File
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.file.{Files, Path, Paths}
import java.util.Collections

import javax.tools.ToolProvider

import scala.collection.{mutable, parallel}
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.reflect.internal.pickling.PickleBuffer
import scala.reflect.internal.util.FakePos
import scala.reflect.io.{VirtualDirectory, VirtualFile}
import scala.tools.nsc.backend.{ClassfileInfo, ScalaClass, ScalaRawClass}
import scala.tools.nsc.classpath.{DirectoryClassPath, VirtualDirectoryClassPath, ZipArchiveFileLookup}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.ClassPath
import scala.util.{Failure, Success}

class PipelineMainClass(label: String, parallelism: Int, strategy: BuildStrategy) {
  private val pickleCacheConfigured = System.getProperty("scala.pipeline.picklecache")
  private val pickleCache = {
    new PickleCache(if (pickleCacheConfigured == null) Files.createTempDirectory("scala.picklecache") else Paths.get(pickleCacheConfigured))
  }

  /** Forward errors to the (current) reporter. */
  protected def scalacError(msg: String): Unit = {
    reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  private var reporter: Reporter = _

  private object handler extends UncaughtExceptionHandler {
    override def uncaughtException(t: Thread, e: Throwable): Unit = {
      e.printStackTrace()
      System.exit(-1)
    }
  }

  implicit val executor = ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(parallelism), t => handler.uncaughtException(Thread.currentThread(), t))
  val fileManager = ToolProvider.getSystemJavaCompiler.getStandardFileManager(null, null, null)

  def pickleClassPath[G <: Global](output: AbstractFile, data: mutable.AnyRefMap[G#Symbol, PickleBuffer]): ClassPath = {
    val cachePath: Path = pickleCache.cachePath(output)
    Files.createDirectories(cachePath)
    val dir = AbstractFile.getDirectory(cachePath.toFile)

    val dirs = mutable.Map[G#Symbol, AbstractFile]()
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
      val primary = base.fileNamed(symbol.encodedName + ".class")
      pickleCache.put(primary, ScalaClass(symbol.fullNameString, () => ByteBuffer.wrap(pickle.bytes)))

      // TODO is this needed?
      //      if (symbol.companionModule.exists) {
      //        val secondary = base.fileNamed(symbol.companionModule.encodedName + "$.class")
      //        pickleCache.put(secondary, ScalaRawClass(symbol.companionModule.fullNameString))
      //      }
    }
    DirectoryClassPath(dir.file)
  }

  private val allPickleData = new java.util.concurrent.ConcurrentHashMap[Path, ClassPath]

  def process(args: Array[String]): Boolean = {
    println(s"parallelism = $parallelism, strategy = $strategy")

    reporter = new ConsoleReporter(new Settings(scalacError))

    def commandFor(argFileArg: String): Task = {
      val ss = new Settings(scalacError)
      val command = new CompilerCommand(("@" + argFileArg) :: Nil, ss)
      Task(argFileArg, command, command.files)
    }

    val projects: List[Task] = args.toList.map(commandFor)
    val produces = mutable.HashMap[Path, Task]()
    for (p <- projects) {
      produces(p.outputDir) = p
    }
    val dependsOn = mutable.HashMap[Task, List[Task]]()
    for (p <- projects) {
      dependsOn(p) = (p.classPath ++ p.pluginClassPath).flatMap(s => produces.get(s)).toList.filterNot(_ == p)
    }
    val dependedOn: Set[Task] = dependsOn.valuesIterator.flatten.toSet

    val timer = new Timer
    timer.start()
    strategy match {
      case OutlineTypePipeline =>
        projects.foreach { p =>
          val isLeaf = !dependedOn.contains(p)
          val depsReady = Future.sequence(dependsOn.getOrElse(p, Nil).map { task => p.dependencyReadyFuture(task) })
          if (isLeaf) {
            for {
              _ <- depsReady
              _ <- {
                p.outlineDone.complete(Success(()))
                p.fullCompile()
                Future.sequence(p.groups.map(_.done.future))
              }
            } yield {
              p.javaCompile()
            }
          } else {
            for {
              _ <- depsReady
              _ <- {
                p.outlineCompile()
                p.outlineDone.future
              }
              _ <- {
                p.fullCompile()
                Future.sequence(p.groups.map(_.done.future))
              }
            } yield {
              p.javaCompile()
            }

          }
        }

        Await.result(Future.sequence(projects.map(_.compilationDone)), Duration.Inf)
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
        projects.foreach { p =>
          val depsReady = Future.sequence(dependsOn.getOrElse(p, Nil).map(task => p.dependencyReadyFuture(task)))
          for {
            _ <- depsReady
            _ <- {
              p.fullCompileExportPickles()
              // Start javac after scalac has completely finished
              Future.sequence(p.groups.map(_.done.future))
            }
          } yield {
            p.javaCompile()
          }
        }
        Await.result(Future.sequence(projects.map(_.compilationDone)), Duration.Inf)
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
        projects.foreach { p =>
          val f1 = Future.sequence[Unit, List](dependsOn.getOrElse(p, Nil).map(_.javaDone.future))
          f1.flatMap { _ =>
            p.outlineDone.complete(Success(()))
            p.fullCompile()
            Future.sequence(p.groups.map(_.done.future)).map(_ => p.javaCompile())
          }
        }
        Await.result(Future.sequence(projects.map(_.compilationDone)), Duration.Inf)
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

    writeChromeTrace(projects)
    deleteTempPickleCache()
    true
  }

  private def deleteTempPickleCache(): Unit = {
    if (pickleCacheConfigured == null) {
      AbstractFile.getDirectory(pickleCache.dir.toFile).delete()
    }
  }

  private def writeChromeTrace(projects: List[Task]) = {
    val trace = new java.lang.StringBuilder()
    trace.append("""{"traceEvents": [""")
    val sb = new mutable.StringBuilder(trace)

    def durationEvent(name: String, cat: String, t: Timer): String = {
      s"""{"name": "$name", "cat": "$cat", "ph": "X", "ts": ${(t.startMicros).toLong}, "dur": ${(t.durationMicros).toLong}, "pid": 0, "tid": ${t.thread.getId}}"""
    }

    def projectEvents(p: Task): List[String] = {
      val events = List.newBuilder[String]
      if (p.outlineTimer.durationMicros > 0d) {
        val desc = if (strategy == OutlineTypePipeline) "outline-type" else "parser-to-pickler"
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
  }

  case class Group(files: List[String]) {
    val timer = new Timer
    val done = Promise[Unit]()
  }

  private case class Task(argsFile: String, command: CompilerCommand, files: List[String]) {
    val label = argsFile.replaceAll("target/", "").replaceAll("""(.*)/(.*).args""", "$1:$2")
    override def toString: String = argsFile
    def outputDir: Path = command.settings.outputDirs.getSingleOutput.get.file.toPath.toAbsolutePath.normalize()
    private def expand(s: command.settings.PathSetting): List[Path] = {
      ClassPath.expandPath(s.value, expandStar = true).map(s => Paths.get(s).toAbsolutePath.normalize())
    }
    def classPath: Seq[Path] = expand(command.settings.classpath)
    def macroClassPath: Seq[Path] = expand(command.settings.YmacroClasspath)
    def macroClassPathSet: Set[Path] = macroClassPath.toSet
    def pluginClassPath: Set[Path] = {
      def asPath(p: String) = ClassPath split p

      val paths = command.settings.plugin.value filter (_ != "") flatMap (s => asPath(s) map (s => Paths.get(s)))
      paths.toSet
    }
    def dependencyReadyFuture(dependency: Task) = if (macroClassPathSet.contains(dependency.outputDir)) {
      log(s"dependency is on macro classpath, will wait for .class files: ${dependency.label}")
      dependency.javaDone.future
    } else if (pluginClassPath.contains(dependency.outputDir)) {
      log(s"dependency is on plugin classpath, will wait for .class files: ${dependency.label}")
      dependency.javaDone.future
    } else
      dependency.outlineDone.future


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

    var outlineCriticalPathMs = 0d
    var regularCriticalPathMs = 0d
    var fullCriticalPathMs = 0d
    val outlineDone: Promise[Unit] = Promise[Unit]()
    val javaDone: Promise[Unit] = Promise[Unit]()
    def compilationDone: Future[List[Unit]] = Future.sequence(outlineDone.future :: (groups.map(_.done.future) :+ javaDone.future))

    lazy val compiler: Global = {
      val result = newCompiler(command.settings)
      val reporter = result.reporter
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporter.echo(command.getInfoMessage(result))
      result
    }

    def outlineCompile(): Unit = {
      outlineTimer.start()
      log("scalac outline: start")
      command.settings.Youtline.value = true
      command.settings.stopAfter.value = List("pickler")
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.None
      val run1 = new compiler.Run()
      run1 compile files
      val outDir = command.settings.outputDirs.getSingleOutput.get
      allPickleData.put(outDir.file.toPath.toRealPath().normalize(), pickleClassPath(outDir, run1.symData))
      outlineTimer.stop()
      reporter.finish()
      if (reporter.hasErrors) {
        log("scalac outline: failed")
        outlineDone.complete(Failure(new RuntimeException("compile failed")))
      } else {
        log("scalac outline: done")
        outlineDone.complete(Success(()))
      }
    }

    def fullCompile(): Unit = {
      command.settings.Youtline.value = false
      command.settings.stopAfter.value = Nil
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.Normal

      val groupCount = groups.size
      for ((group, ix) <- groups.zipWithIndex) {
        group.done.completeWith {
          Future {
            log(s"scalac (${ix + 1}/$groupCount): start")
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
            log(s"scalac (${ix + 1}/$groupCount): done")
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
            val outDir = command.settings.outputDirs.getSingleOutput.get
            allPickleData.put(outDir.file.toPath.toRealPath().normalize(), pickleClassPath(outDir, symData))
            outlineTimer.stop()
            outlineDone.complete(Success(()))
            group.timer.start()
            log("scalac: exported pickles")
          }
          super.advancePhase()
        }
      }

      try {
        run2 compile group.files
        compiler.reporter.finish()
        group.timer.stop()
        if (compiler.reporter.hasErrors) {
          log("scalac: failed")
          outlineDone.complete(Failure(new RuntimeException("Compile failed")))
          group.done.complete(Failure(new RuntimeException("Compile failed")))
        } else {
          log("scalac: done")
          //        outlineDone.complete(Success(()))
          group.done.complete(Success(()))
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          outlineDone.complete(Failure(new RuntimeException("Compile failed")))
          group.done.complete(Failure(new RuntimeException("Compile failed")))
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
      log("javac: done")
    }
    def log(msg: String): Unit = println(this.label + ": " + msg)
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

    if (strategy != Traditional) {
      val plugin: g.platform.ClassPathPlugin = new g.platform.ClassPathPlugin {
        val replacements = mutable.Buffer[ClassPath]()
        def replaceInternalClassPath(cp: ClassPath, underlying: Path): List[ClassPath] = {
          allPickleData.get(underlying.toRealPath().normalize()) match {
            case null =>
              cp :: Nil
            case pcp =>
              replacements += pcp
              pcp :: Nil
          }
        }
        override def modifyClassPath(classPath: Seq[ClassPath]): Seq[ClassPath] = {
          classPath.flatMap {
            case zcp: ZipArchiveFileLookup[_] => replaceInternalClassPath(zcp, zcp.zipFile.toPath)
            case dcp: DirectoryClassPath => replaceInternalClassPath(dcp, dcp.dir.toPath)
            case cp => cp :: Nil
          }
        }

        override def info(file: AbstractFile, clazz: g.ClassSymbol): Option[ClassfileInfo] = {
          pickleCache.get(file)
        }
        override def parsed(file: AbstractFile, clazz: g.ClassSymbol, info: ClassfileInfo): Unit = {
          pickleCache.put(file, info)
        }
      }
      g.platform.addClassPathPlugin(plugin)
    }
    g
  }

}

sealed abstract class BuildStrategy

/** Outline type check to compute type signatures as pickles as an input to downstream compilation. */
case object OutlineTypePipeline extends BuildStrategy

case object Pipeline extends BuildStrategy

/** Emit class files before triggering downstream compilation */
case object Traditional extends BuildStrategy

object PipelineMain {
  def main(args: Array[String]): Unit = {
    val strategies = List(OutlineTypePipeline, Pipeline, Traditional)
    val strategy = strategies.find(_.productPrefix.equalsIgnoreCase(System.getProperty("scala.pipeline.strategy", "pipeline"))).get
    val parallelism = java.lang.Integer.getInteger("scala.pipeline.parallelism", parallel.availableProcessors)
    val main = new PipelineMainClass("1", parallelism, strategy)
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
      val result = main.process(Array(
        "/code/boxer/macros/target/compile.args",
        "/code/boxer/plugin/target/compile.args",
        "/code/boxer/support/target/compile.args",
        "/code/boxer/use-macro/target/compile.args",
        "/code/boxer/use-plugin/target/compile.args"))
      if (!result)
        System.exit(1)
    }
    System.exit(0)
  }
}

class PickleCache(val dir: Path) {

  private val PicklePattern = """(.*)\.pickle""".r
  private val RawPattern = """(.*)\.raw""".r
  def get(file: AbstractFile): Option[ClassfileInfo] = synchronized {
    val cachePath = this.cachePath(file)
    if (Files.exists(cachePath)) {
      val listing = Files.list(cachePath)
      try {
        val it = listing.iterator()
        if (it.hasNext) {
          val f = it.next()
          val name = f.getFileName
          name.toString match {
            case PicklePattern(className) =>
              val bytes = Files.readAllBytes(f)
              Some(ScalaClass(className, () => ByteBuffer.wrap(bytes)))
            case RawPattern(className) =>
              val bytes = Files.readAllBytes(f)
              Some(backend.ScalaRawClass(className))
            case _ => None
          }
        } else None
      } finally {
        listing.close()
      }
    } else None
  }
  def cachePath(file: AbstractFile): Path = {
    file.underlyingSource match {
      case Some(jar) if jar ne file =>
        dir.resolve("." + jar.file.toPath).normalize().resolve(file.path)
      case _ =>
        dir.resolve("./" + file.path).normalize()
    }
  }

  def put(file: AbstractFile, info: ClassfileInfo): Unit = {
    val cachePath = this.cachePath(file)
    info match {
      case ScalaClass(className, pickle) =>
        Files.createDirectories(cachePath)
        val ch = Channels.newChannel(Files.newOutputStream(cachePath.resolve(className + ".pickle")))
        try ch.write(pickle())
        finally ch.close()
      case ScalaRawClass(className) =>
        Files.createDirectories(cachePath)
        Files.write(cachePath.resolve(className + ".raw"), Array[Byte]())
      case _ =>
    }
  }
}