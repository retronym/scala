package scala.tools.nsc

import java.nio.file.Files

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.jdk.CollectionConverters._
import scala.tools.testkit.ReleasablePath._
import scala.util.Using

class CompilerCommandTest {
  private def expand(lines: String*): List[String] =
    Using.resource(Files.createTempFile("argfile", ".txt")) { file =>
      Files.write(file, lines.asJava)
      val errors = List.newBuilder[String]
      val command = new CompilerCommand(Nil, new Settings(errors += _))
      val args = command.expandArg(s"@$file")
      assertEquals(Nil, errors.result())
      args
    }

  @Test def `hash starts a comment at the start of an argument`(): Unit = {
    assertEquals(List("-deprecation", "-feature", "A.scala"),
      expand("# options", "-deprecation # warn", "", "  # indented", "-feature\t#tab", "A.scala #", "#"))
  }

  @Test def `hash inside an argument is not a comment`(): Unit = {
    assertEquals(List("-Wconf:msg=C#:e", "src/Dep.scala"), expand("-Wconf:msg=C#:e src/Dep.scala"))
    assertEquals(List("-d", "out#dir", "src/Ok.scala"), expand("-d out#dir src/Ok.scala"))
    assertEquals(List("a#", "b"), expand("a# b # c"))
  }

  @Test def `hash inside quotes is not a comment`(): Unit = {
    assertEquals(List("-Wconf:msg=a # b:e", "src/Dep.scala"), expand(""""-Wconf:msg=a # b:e" src/Dep.scala"""))
    assertEquals(List("-Wconf:msg=a # b:e", "src/Dep.scala"), expand("""'-Wconf:msg=a # b:e' src/Dep.scala # c"""))
    assertEquals(List("-Wconf:msg=a # b:e"), expand("""-Wconf:"msg=a # b":e"""))
    assertEquals(List("# a", "b"), expand(""""# a" b #c"""))
    assertEquals(List("""a\"""", "b"), expand("""a\" b # " c"""))
  }
}
