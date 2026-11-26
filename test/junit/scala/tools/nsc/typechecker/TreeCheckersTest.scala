package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class TreeCheckersTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  import compiler._
  import global._

  /** Typechecks `code`, applies `mutate` to the typed trees, runs the tree checker after typer and returns its scope reports. */
  private def scopeReports(code: String)(mutate: Tree => Unit = _ => ()): List[String] = {
    val run = newRun()
    run.compileSources(makeSourceFile(code, "UnitTestSource.scala") :: Nil)
    checkReport()
    exitingTyper {
      run.units.foreach(u => mutate(u.body))
      treeChecker.checkTrees()
    }
    run.units.foreach(run.reporting.reportSuspendedMessages)
    storeReporter.infos.toList.map(_.msg).filter(_.contains("Out of scope symbol reference"))
  }

  private def storeReporter = global.reporter.asInstanceOf[scala.tools.nsc.reporters.StoreReporter]

  @Test def boundTypeParamsAreInScope(): Unit = {
    val code =
      """object T {
        |  def lub(b: Boolean) = if (b) List(1) else Vector(1)
        |  def refinement: { def m[A](a: A): List[A] } = new { def m[A](a: A): List[A] = List(a) }
        |}
        |""".stripMargin
    assertEquals(Nil, scopeReports(code)())
  }

  // A method whose info reuses another method's type parameters, as an info copied without cloning them would.
  @Test def sharedTypeParamsAreOutOfScope(): Unit = {
    val code =
      """object T {
        |  def f[A](a: A): A = a
        |  def g[B](b: B): B = b
        |}
        |""".stripMargin
    val reports = scopeReports(code) { body =>
      val defs = body.collect { case dd: DefDef if Set("f", "g")(dd.name.toString) => dd.name.toString -> dd.symbol }.toMap
      defs("g").setInfo(defs("f").info)
    }
    assertEquals(reports.mkString("\n"), 1, reports.length)
    assertTrue(reports.head, reports.head.contains("ref to") && reports.head.linesIterator.exists(l => l.contains("ref to") && l.contains(" A ")))
  }
}
