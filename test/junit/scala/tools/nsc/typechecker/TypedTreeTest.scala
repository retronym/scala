package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.BytecodeTesting

class TypedTreeTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  @Test
  def constantFoldedOriginalTreeAttachment(): Unit = {
    val code =
      """object O {
        |  final val x = 42
        |  def f(x: Int) = x
        |  def f(x: Boolean) = x
        |  f(O.x)
        |}
      """.stripMargin
    val run = compiler.newRun
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    val List(t) = tree.filter(_.attachments.all.nonEmpty).toList
    assertEquals(s"$t:${t.attachments.all}", "42:Set(OriginalTreeAttachment(O.x))")
  }

  @Test
  def constantFoldingTest(): Unit = {
    import compiler.global._
    def check(code: String, expected: Constant) = {
      def wrap(const: String) =
        s"""package p1
           |class C {
           |  final val OneFloat = 1f
           |  final val OneLong = 1L
           |  final val OneInt = 1
           |  final val OneDouble = 1d
           |  final val OneChar = '1'
           |  final val True = true
           |  final val a = $const
           |}
         """.stripMargin
      val run = compiler.newRun
      run.compileSources(List(BytecodeTesting.makeSourceFile(wrap(code), "UnitTestSource.scala")))
      Assert.assertTrue(reporter.asInstanceOf[StoreReporter].infos.mkString("\n"), !reporter.hasErrors)
      val symbol = rootMirror.getRequiredClass("p1.C")
      val info = exitingTyper(symbol.info)
      val member = info.decl(TermName("a"))
      if (member == NoSymbol) Assert.fail("Could not find a in " + info)
      member.info.resultType match {
        case ct : ConstantType =>
          // note, Constant(42) != Constant(42L). That's the right semantics for our test.
          assertEquals("Wrong type for " + member.defString, expected, ct.value)
        case _ => Assert.fail(member.defString + " not a constant")
      }
    }
    check("42", Constant(42))
    check("-42", Constant(-42))
    check("42L >>> 2", Constant(42L >>> 2))
    check("1 + 1", Constant(1 + 1))
    check("2 - 1", Constant(2 - 1))
    check("2f * 1d", Constant(2f * 1d))
    check("(2f * 1d) / 2", Constant((2f * 1d) / 2))
    check("True || false", Constant(true | false))
    check("OneFloat", Constant(1f))
    check("OneInt", Constant(1))
    check("OneDouble", Constant(1d))
    check("OneChar", Constant('1'))

    check("~42", Constant(~42))
    // TODO treat Typed(Literal, _) as a constant if it is a numeric widening or narrowing
  }
}
