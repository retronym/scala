package scala.tools.nsc.typechecker

import org.junit.Assert.assertNotNull
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class ForceAnalyzerLazyValsTest {
  /** A worker that forces this lazy val holds the analyzer's monitor while it waits for a claim, and a thread that holds
   *  the claim may wait for the monitor: a deadlock that the claims cannot see. No case class is compiled here, so only
   *  forcing the analyzer's lazy vals before the workers start can have set it. */
  @Test def theAnalyzersLazyValsAreForcedBeforeTheWorkersStart(): Unit = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = "-Yparallel-typer -Yparallel-typer-threads 2")
    // two units: the number of workers is at most the number of units, and one worker forces nothing
    compiler.newRun().compileSources(List(BytecodeTesting.makeSourceFile("class C", "C.scala"), BytecodeTesting.makeSourceFile("class D", "D.scala")))
    val analyzer = compiler.global.analyzer
    val field = analyzer.getClass.getDeclaredFields.find(_.getName.endsWith("caseObjectSymbols")).get
    field.setAccessible(true)
    assertNotNull(field.get(analyzer))
  }
}
