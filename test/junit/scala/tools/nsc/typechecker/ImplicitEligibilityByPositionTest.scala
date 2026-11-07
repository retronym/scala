package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class ImplicitEligibilityByPositionTest {
  private def compiles(options: String, code: String): Boolean = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = options)
    compiler.newRun().compileSources(List(BytecodeTesting.makeSourceFile(code, "Test.scala")))
    !compiler.global.reporter.hasErrors
  }

  /** `y` completes `foo` before `a` is typed, so without the option `foo` is eligible for `a`. */
  private val forcedEarlier =
    """class Foo
      |object O {
      |  val y = foo
      |  def a: Foo = implicitly[Foo]
      |  implicit val foo = new Foo
      |}""".stripMargin

  private def code(definition: String) =
    s"""class Foo
       |object O {
       |  def a: Foo = implicitly[Foo]
       |  $definition
       |}""".stripMargin

  private val byPosition = "-Yimplicit-eligibility-by-position"

  @Test def completedEarlierIsEligibleWithoutTheOption(): Unit =
    assertEquals(true, compiles("", forcedEarlier))

  @Test def completedEarlierIsNotEligibleWithIt(): Unit =
    assertEquals(false, compiles(byPosition, forcedEarlier))

  @Test def impliedByParallelTyper(): Unit =
    assertEquals(false, compiles("-Yparallel-typer", forcedEarlier))

  @Test def canBeTurnedOffUnderParallelTyper(): Unit =
    assertEquals(true, compiles(s"-Yparallel-typer $byPosition:false", forcedEarlier))

  @Test def laterValWithoutATypeIsNotEligible(): Unit =
    assertEquals(false, compiles(byPosition, code("implicit val foo = new Foo")))

  @Test def laterDefWithoutATypeIsNotEligible(): Unit =
    assertEquals(false, compiles(byPosition, code("implicit def foo = new Foo")))

  @Test def laterValWithATypeIsEligible(): Unit =
    assertEquals(true, compiles(byPosition, code("implicit val foo: Foo = new Foo")))

  @Test def laterDefWithATypeIsEligible(): Unit =
    assertEquals(true, compiles(byPosition, code("implicit def foo: Foo = new Foo")))

  @Test def localValWithoutATypeIsEligibleAfterIt(): Unit =
    assertEquals(true, compiles(byPosition,
      """class Foo
        |object O {
        |  def a: Foo = {
        |    implicit val foo = new Foo
        |    implicitly[Foo]
        |  }
        |}""".stripMargin))

  @Test def earlierValWithoutATypeIsEligible(): Unit =
    assertEquals(true, compiles(byPosition,
      """class Foo
        |object O {
        |  implicit val foo = new Foo
        |  def a: Foo = implicitly[Foo]
        |}""".stripMargin))
}
