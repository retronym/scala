package scala.tools.nsc

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Files

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

import scala.tools.testkit.ReleasablePath._
import scala.util.Using

class MainTest {
  // result of `process` and the console output; `process` rethrows unexpected exceptions
  private def run(args: String*): (Boolean, String) = {
    val out = new ByteArrayOutputStream
    val ps  = new PrintStream(out, true)
    val ok  = Console.withOut(ps)(Console.withErr(ps)(new MainClass().process(args.toArray)))
    (ok, out.toString)
  }
  private def assertFails(message: String)(args: String*): Unit = {
    val (ok, out) = run(args: _*)
    assertFalse(out, ok)
    assertTrue(out, out.contains(message))
  }

  @Test def `unreadable argument file is an error`(): Unit =
    Using.resource(Files.createTempDirectory("main-test")) { tmp =>
      val missing = tmp.resolve("missing")
      assertFails(s"scalac error: argument file $missing could not be found")(s"@$missing")
      assertFails(s"scalac error: argument file ${tmp} could not be read")(s"@${tmp}")
    }

  @Test def `unwritable output of an option is an error`(): Unit =
    Using.resource(Files.createTempDirectory("main-test")) { tmp =>
      val src = tmp.resolve("C.scala")
      Files.write(src, "class C".getBytes)
      val missing = tmp.resolve("missing")
      assertFails(s"scalac error: cannot write to -Yprofile-destination ${missing.resolve("p.csv")}")(
        "-usejavacp", "-d", tmp.toString, "-Yprofile-destination", missing.resolve("p.csv").toString, src.toString)
      assertFails(s"scalac error: cannot write to -Yprofile-destination $tmp")(
        "-usejavacp", "-d", tmp.toString, "-Yprofile-destination", tmp.toString, src.toString)
      assertFails("error: error writing phase graph")(
        "-usejavacp", "-d", tmp.toString, "-Vphase-graph", "missing/x")
    }
}
