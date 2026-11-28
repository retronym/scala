package scala.tools.nsc.transform

import org.junit.Assert.assertTrue
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class TypeAssignerTest extends BytecodeTesting {
  // Before erasure, `C.super` has type `C.super.type`, as the typer gives it, not the type of the selected
  // member's owner: explicitouter would otherwise take the trait `T` for the qualifier's class and make the
  // protected member public.
  @Test def superSelectionBeforeErasureKeepsSuperType(): Unit = {
    val global = compiler.global
    import global._
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(
      """trait T { protected lazy val v = new Object }
        |class C extends T { def f = v }
        |""".stripMargin, "C.scala")))
    compiler.checkReport()
    val v = exitingPhase(run.explicitouterPhase)(rootMirror.EmptyPackageClass.info.decl(TypeName("T")).info.decl(TermName("v")))
    assertTrue(v.toString, exitingPhase(run.explicitouterPhase)(v.isProtected))
  }
}
