package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class LineNumberTest extends BytecodeTesting {

  import compiler._

  @Test
  def lineNumberSynthetics(): Unit = {
    // Checking that minimalRemoveUnreachableCode doesn't eliminate the local variable
    // enties for a, a1
    val code                    =
      """
        |class Test {
        |  def main(): Unit = {
        |    for {
        |      a <- Seq(1)
        |      a1 = a + 1
        |    } {
        |      def getA = a
        |      def getA1 = a1
        |      println("")
        |    }
        |  }
        |}
        |""".stripMargin
    val cls = compileClass(code)
    val m = getMethod(cls, "$anonfun$main$2")
    assertEquals(m.localVars.toString, m.localVars.map(_.name).sorted, List("a", "a1", "x$1"))
  }

  // Line numbers of a method in bytecode order. Line 1 is the first line of `code`.
  private def lines(code: String, method: String): List[Int] =
    getInstructions(compileClasses(code).find(_.name == "C").get, method).collect { case LineNumber(n, _) => n }

  // Code generated for testing a case (reading the scrutinee or a shared extractor result) belongs to the
  // line of that case. Stepping through the match in a debugger must not go back to the selector.
  @Test
  def patmatSharedExtractor(): Unit = {
    val code =
      """class C {
        |  object Even { def unapply(x: Int): Option[Int] = if (x % 2 == 0) Some(x / 2) else None }
        |  def f(x: Int): Int =
        |    x match {
        |      case Even(1) => 1
        |      case Even(2) => 2
        |      case Even(n) => n
        |      case _ => 0
        |    }
        |}
        |""".stripMargin
    assertEquals(List(4, 5, 6, 7, 8), lines(code, "f"))
  }

  @Test
  def patmatGuardReadsBinder(): Unit = {
    val code =
      """class C {
        |  def cond(x: Any): Boolean = x.hashCode < 0
        |  def f(x: Any): Int =
        |    x match {
        |      case y if cond(y) => 1
        |      case z => 2
        |    }
        |}
        |""".stripMargin
    assertEquals(List(4, 5, 6), lines(code, "f"))
  }

  @Test
  def patmatCommonSubconditions(): Unit = {
    // the type test of `Some` is shared, its result is stored in a variable hoisted before the match
    val code =
      """class C {
        |  def f(x: Option[Any]): Int =
        |    x match {
        |      case Some(1) => 1
        |      case Some(s: String) if s.isEmpty => 2
        |      case Some(s: String) => 3
        |      case _ => 4
        |    }
        |}
        |""".stripMargin
    assertEquals(List(3, 4, 5, 6, 7), lines(code, "f"))
  }

  @Test
  def patmatSwitchAndMatchError(): Unit = {
    // switch dispatch and the MatchError belong to the selector line
    val switch =
      """class C {
        |  def f(x: Int): Int =
        |    x match {
        |      case 1 =>
        |        10
        |      case 2 =>
        |        20
        |      case 3 =>
        |        30
        |    }
        |}
        |""".stripMargin
    assertEquals(List(3, 5, 7, 9, 3), lines(switch, "f"))
    val nonSwitch =
      """class C {
        |  def f(x: Option[Int]): Int =
        |    (x: @unchecked) match {
        |      case Some(1) => 1
        |      case None => 0
        |    }
        |}
        |""".stripMargin
    assertEquals(List(3, 4, 5, 3), lines(nonSwitch, "f"))
  }
}
