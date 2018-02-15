package partest

import java.io.{File, OutputStream}
import java.lang.reflect.InvocationTargetException
import java.nio.charset.Charset
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.Properties

import org.junit.Assert

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc
import scala.tools.partest.TestState._
import scala.tools.partest.{TestState, file2String, nest}
import scala.tools.partest.nest.{ConsoleRunner, RunnerSpec}

class JUnitPartest {
  def compileOnly = false
  def runTest(path: String): Unit = {
    val methodName = Thread.currentThread().getStackTrace.apply(2).getMethodName

    val here = Paths.get(".").toAbsolutePath
    val parents = collection.Iterator.iterate(here)(_.getParent).takeWhile(_ != null)
    def cannotFindFile(): Nothing = throw new IllegalArgumentException("Cannot resolve " + path + " from "  + here + " or any of its parents")
    val testFile = parents.map(_.resolve(path)).find(Files.exists(_)).getOrElse(cannotFindFile()).toAbsolutePath().normalize()
    val consoleRunner = new ConsoleRunner(RunnerSpec.forArgs(Array()))
    def execTestInProcess(classesDir: File, log: File, properties: Map[String, String]): Boolean = {
      for ((k, v) <- properties) System.setProperty(k, v)
      def run(): Unit = {
        val loader = new URLClassLoader(classesDir.toURI.toURL :: Nil, getClass.getClassLoader)
        val cls = loader.loadClass("Test")
        val main = cls.getDeclaredMethod("main", classOf[Array[String]])
        withExtraProperties(properties) {
          val out = Files.newOutputStream(log.toPath, StandardOpenOption.APPEND)
          try {
            capturingOutErr(out) {
              try {
                main.invoke(null, Array[String]("jvm"))
              } catch {
                case ite: InvocationTargetException => throw ite.getCause
              }
            }
          } finally {
            out.close()
          }
        }
      }

      TrapExit(() => run()) match {
        case Left((status, throwable)) if status != 0 =>
          Files.readAllLines(log.toPath).forEach(println(_))
          val error = new AssertionError(s"System.exit(${status}) was called.")
          error.setStackTrace(throwable.getStackTrace)
          throw error
        case _ =>
      }
      true
    }

    val runner = new nest.Runner(testFile.toFile, consoleRunner.suiteRunner, consoleRunner.nestUI) {
      override def run(): TestState = {
        // javac runner, for one, would merely append to an existing log file, so just delete it before we start
        logFile.delete()
        val argsFile  = nsc.io.Path(testFile).changeExtension("javaopts").jfile
        val argString = file2String(argsFile)
        if (!compileOnly && argString != "")
          super.run() // we need to fork execution of this test as it has custom JVM options
        else
          runTestCommon((if (compileOnly) true else execTestInProcess(outDir, logFile, propertyOptions) && diffIsOk))

        lastState
      }
      import consoleRunner.suiteRunner.fileManager._
      import consoleRunner.suiteRunner._
      val testFullPath = testFile.getAbsolutePath
      def propertyOptions = Map[String, String](
        "file.encoding" -> "UTF-8",
        "java.library.path" -> ("" +logFile.getParentFile.getAbsolutePath),
        "partest.output" -> (""+outDir.getAbsolutePath),
        "partest.lib" -> (""+libraryUnderTest.jfile.getAbsolutePath),
        "partest.reflect" -> (""+reflectUnderTest.jfile.getAbsolutePath),
        "partest.comp" -> (""+compilerUnderTest.jfile.getAbsolutePath),
        "partest.cwd" -> (""+outDir.getParent),
        "partest.test-path" -> (""+testFullPath),
        "partest.testname" -> (""+fileBase),
        "javacmd" -> (""+javaCmdPath),
        "javaccmd" -> (""+javacCmdPath),
        "user.language" -> "en",
        "user.country" -> "US"
      )

    }
    runner.run()
    runner.lastState match {
      case Pass(testFile) =>
      case Uninitialized(testFile) =>
        Assert.fail("Uninitialized")
      case Updated(testFile) =>
        println("updated")
      case Skip(testFile, reason) =>
        println("skipped")
      case Fail(testFile, reason, transcript) =>
        transcript.foreach(println)
        Assert.fail(reason)
      case Crash(testFile, caught, transcript) =>
        transcript.foreach(println)
        throw caught
    }
  }

  private def withExtraProperties[A](extra: Map[String, String])(action: => A): A = {
    val saved = System.getSecurityManager
    val savedProps = new Properties()
    System.getProperties().stringPropertyNames.forEach((k) => savedProps.setProperty(k, System.getProperty(k)))
    try {
      action
    } finally {
      System.setProperties(savedProps)
    }
  }


  private def capturingOutErr[A](output: OutputStream)(f: => A): A = {
    import java.io._
    val savedOut = System.out
    val savedErr = System.err
    try {
      val charset = Charset.defaultCharset()
      val printStream = new PrintStream(output, true, charset.name())
      try {
        System.setOut(printStream)
        System.setErr(printStream)
        scala.Console.withErr(printStream) {
          scala.Console.withOut(printStream) {
            f
          }
        }
      } finally {
        printStream.close()
      }
    } finally {
      System.setOut(savedOut)
      System.setOut(savedErr)
    }
  }
}
