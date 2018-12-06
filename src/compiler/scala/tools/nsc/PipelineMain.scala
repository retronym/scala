/* NSC -- new Scala compiler
 * Copyright 2005-2018 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc

import java.io.File
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.Collections

import javax.tools.ToolProvider

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.{mutable, parallel}
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.reflect.internal.pickling.PickleBuffer
import scala.reflect.internal.util.FakePos
import scala.reflect.io.RootPath
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.ClassPath
import scala.util.{Failure, Success}

class PipelineMainClass(label: String, parallelism: Int, strategy: BuildStrategy, argFiles: Seq[Path]) {
  private val pickleCacheConfigured = System.getProperty("scala.pipeline.picklecache")
  private val pickleCache: Path = {
    if (pickleCacheConfigured == null) Files.createTempDirectory("scala.picklecache") else Paths.get(pickleCacheConfigured)
  }
  private def cachePath(file: Path): Path = pickleCache.resolve("./" + file).normalize()

  private val strippedAndExportedClassPath = mutable.HashMap[Path, Path]()

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
  def changeExtension(p: Path, newExtension: String): Path = {
    val fileName = p.getFileName.toString
    val changedFileName = fileName.lastIndexOf('.') match {
      case -1 => fileName + newExtension
      case n => fileName.substring(0, n) + newExtension
    }
    p.getParent.resolve(changedFileName)
  }

  def registerPickleClassPath[G <: Global](output: Path, data: mutable.AnyRefMap[G#Symbol, PickleBuffer]): Unit = {
    val jarPath = changeExtension(cachePath(output), ".jar")
    val root = RootPath(jarPath, writable = true)

    val dirs = mutable.Map[G#Symbol, Path]()
    def packageDir(packSymbol: G#Symbol): Path = {
      if (packSymbol.isEmptyPackageClass) root.root
      else if (dirs.contains(packSymbol)) dirs(packSymbol)
      else if (packSymbol.owner.isRoot) {
        val subDir = root.root.resolve(packSymbol.encodedName)
        Files.createDirectories(subDir)
        dirs.put(packSymbol, subDir)
        subDir
      } else {
        val base = packageDir(packSymbol.owner)
        val subDir = base.resolve(packSymbol.encodedName)
        Files.createDirectories(subDir)
        dirs.put(packSymbol, subDir)
        subDir
      }
    }
    val written = new java.util.IdentityHashMap[AnyRef, Unit]()
    try {
      for ((symbol, pickle) <- data) {
        if (!written.containsKey(pickle)) {
          val base = packageDir(symbol.owner)
          val primary = base.resolve(symbol.encodedName + ".sig")
          Files.write(primary, pickle.bytes)
          written.put(pickle, ())
        }
      }
    } finally {
      root.close()
    }
    Files.setLastModifiedTime(jarPath, FileTime.from(Instant.now()))
    strippedAndExportedClassPath.put(output.toRealPath().normalize(), jarPath)
  }


  def writeDotFile(dependsOn: mutable.LinkedHashMap[Task, List[Dependency]]): Unit = {
    val builder = new java.lang.StringBuilder()
    builder.append("digraph projects {\n")
    for ((p, deps) <- dependsOn) {
      //builder.append("  node \"[]").append(p.label).append("\";\n")
      for (dep <- deps) {
        builder.append("   \"").append(p.label).append("\" -> \"").append(dep.t.label).append("\" [")
        if (dep.isMacro) builder.append("label=M")
        else if (dep.isPlugin) builder.append("label=P")
        builder.append("];\n")
      }
    }
    builder.append("}\n")
    val path = Paths.get("projects.dot")
    Files.write(path, builder.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println("Wrote project dependency graph to: " + path.toAbsolutePath)
  }

  private case class Dependency(t: Task, isMacro: Boolean, isPlugin: Boolean)

  def process(): Boolean = {
    println(s"parallelism = $parallelism, strategy = $strategy")

    reporter = new ConsoleReporter(new Settings(scalacError))

    def commandFor(argFileArg: Path): Task = {
      val ss = new Settings(scalacError)
      val command = new CompilerCommand(("@" + argFileArg) :: Nil, ss)
      Task(argFileArg, command, command.files)
    }

    val projects: List[Task] = argFiles.toList.map(commandFor)
    val produces = mutable.LinkedHashMap[Path, Task]()
    for (p <- projects) {
      produces(p.outputDir) = p
    }
    val dependsOn = mutable.LinkedHashMap[Task, List[Dependency]]()
    for (p <- projects) {
      val macroDeps = p.macroClassPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).map(t => Dependency(t, isMacro = true, isPlugin = false))
      val pluginDeps = p.pluginClassPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).map(t => Dependency(t, isMacro = false, isPlugin = true))
      val classPathDeps = p.classPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).filterNot(p => macroDeps.exists(_.t == p)).map(t => Dependency(t, isMacro = false, isPlugin = false))
      dependsOn(p) = classPathDeps ++ macroDeps ++ pluginDeps
    }
    val dependedOn: Set[Task] = dependsOn.valuesIterator.flatten.map(_.t).toSet
    val externalClassPath = projects.iterator.flatMap(_.classPath).filter(p => !produces.contains(p) && Files.exists(p)).toSet

    if (strategy != Traditional) {
      val exportTimer = new Timer
      exportTimer.start()
      for (entry <- externalClassPath) {
        val extracted = cachePath(entry)
        val sourceTimeStamp = Files.getLastModifiedTime(entry)
        if (Files.exists(extracted) && Files.getLastModifiedTime(extracted) == sourceTimeStamp) {
          // println(s"Skipped export of pickles from $entry to $extracted (up to date)")
        } else {
          PickleExtractor.process(entry, extracted)
          Files.setLastModifiedTime(extracted, sourceTimeStamp)
          println(s"Exported pickles from $entry to $extracted")
          Files.setLastModifiedTime(extracted, sourceTimeStamp)
        }
        strippedAndExportedClassPath(entry) = extracted
      }
      exportTimer.stop()
      println(f"Exported external classpath in ${exportTimer.durationMs}%.0f ms")
    }

    writeDotFile(dependsOn)

    val timer = new Timer
    timer.start()

    def awaitDone(): Unit = {
      Await.result(Future.sequence(projects.map(_.compilationDone)), Duration.Inf)
      timer.stop()
    }
    strategy match {
      case OutlineTypePipeline =>
        projects.foreach { p =>
          val isLeaf = !dependedOn.contains(p)
          val depsReady = Future.sequence(dependsOn.getOrElse(p, Nil).map { task => p.dependencyReadyFuture(task.t) })
          val f = if (isLeaf) {
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
          f.onComplete { _ => p.compiler.close() }
        }

        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

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
          val depsReady = Future.sequence(dependsOn.getOrElse(p, Nil).map(task => p.dependencyReadyFuture(task.t)))
          val f = for {
            _ <- depsReady
            _ <- {
              p.fullCompileExportPickles()
              // Start javac after scalac has completely finished
              Future.sequence(p.groups.map(_.done.future))
            }
          } yield {
            p.javaCompile()
          }
          f.onComplete { _ => p.compiler.close() }
        }
        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

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
          val f1 = Future.sequence(dependsOn.getOrElse(p, Nil).map(_.t.javaDone.future))
          val f2 = f1.flatMap { _ =>
            p.outlineDone.complete(Success(()))
            p.fullCompile()
            Future.sequence(p.groups.map(_.done.future)).map(_ => p.javaCompile())
          }
          f2.onComplete { _ => p.compiler.close() }
        }
        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

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
      AbstractFile.getDirectory(pickleCache.toFile).delete()
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

  private case class Task(argsFile: Path, command: CompilerCommand, files: List[String]) {
    val label = argsFile.toString.replaceAll("target/", "").replaceAll("""(.*)/(.*).args""", "$1:$2")
    override def toString: String = argsFile.toString
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


    command.settings.YcacheMacroClassLoader.value = "always"
    command.settings.YcachePluginClassLoader.value = "always"

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

    val originalClassPath: String = command.settings.classpath.value

    lazy val compiler: Global = try {
      val result = newCompiler(command.settings)
      val reporter = result.reporter
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporter.echo(command.getInfoMessage(result))
      result
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }

    def outlineCompile(): Unit = {
      outlineTimer.start()
      try {
        log("scalac outline: start")
        command.settings.Youtline.value = true
        command.settings.stopAfter.value = List("pickler")
        command.settings.Ymacroexpand.value = command.settings.MacroExpand.None
        val run1 = new compiler.Run()
        run1 compile files
        registerPickleClassPath(command.settings.outputDirs.getSingleOutput.get.file.toPath, run1.symData)
        outlineTimer.stop()
        reporter.finish()
        if (reporter.hasErrors) {
          log("scalac outline: failed")
          outlineDone.complete(Failure(new RuntimeException("compile failed")))
        } else {
          log("scalac outline: done")
          outlineDone.complete(Success(()))
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          outlineDone.complete(Failure(new RuntimeException("compile failed")))
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
            try {
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
            } finally {
              compiler2.close()
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
      try {
        val run2 = new compiler.Run() {
          override def advancePhase(): Unit = {
            if (compiler.phase == this.picklerPhase) {
              registerPickleClassPath(command.settings.outputDirs.getSingleOutput.get.file.toPath, symData)
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
          val opts = java.util.Arrays.asList("-d", command.settings.outdir.value, "-cp", command.settings.outdir.value + File.pathSeparator + originalClassPath)
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
    if (strategy != Traditional) {
      val classPath = ClassPath.expandPath(settings.classpath.value, expandStar = true)
      val modifiedClassPath = classPath.map { entry =>
        val entryPath = Paths.get(entry)
        if (Files.exists(entryPath))
          strippedAndExportedClassPath.getOrElse(entryPath.toRealPath().normalize(), entryPath).toString
        else
          entryPath
      }
      settings.classpath.value = modifiedClassPath.mkString(java.io.File.pathSeparator)
    }
    Global(settings)
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
    val argFiles: Seq[Path] = args match {
      case Array(path) if Files.isDirectory(Paths.get(path)) =>
        Files.walk(Paths.get(path)).iterator().asScala.filter(_.getFileName.toString.endsWith(".args")).toList
      case _ =>
        args.map(Paths.get(_))
    }
    val main = new PipelineMainClass("1", parallelism, strategy, argFiles)
    val result = main.process()
    if (!result)
      System.exit(1)
    else
      System.exit(0)
  }
}

object PipelineMainTest {
  def main(args: Array[String]): Unit = {
    var i = 0
    val argsFiles = Files.walk(Paths.get("/code/guardian-frontend")).iterator().asScala.filter(_.getFileName.toString.endsWith(".args")).toList
    for (_ <- 1 to 10; n <- List(parallel.availableProcessors); strat <- List(Pipeline, OutlineTypePipeline, Traditional)) {
      i += 1
      val main = new PipelineMainClass(i.toString, n, strat, argsFiles)
      println(s"====== ITERATION $i=======")
      val result = main.process()
      if (!result)
        System.exit(1)
    }
    System.exit(0)
  }
}
