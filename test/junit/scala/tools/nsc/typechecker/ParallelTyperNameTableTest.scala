package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.{BytecodeTesting, Compiler}

/** -Yparallel-typer reads names without the name table's lock, which is safe only in the store with a `String` per
 *  name that is shared between compilers. */
class ParallelTyperNameTableTest {
  private def compile(options: String, units: Int) = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = s"-Yparallel-typer -Yparallel-typer-threads 4 $options")
    // Each unit refers to the next one's class, companion and members, so that the threads make, and look up, the
    // same term and type names.
    val sources = (0 until units).map { i =>
      val j = (i + 1) % units
      BytecodeTesting.makeSourceFile(
        s"""class C$i { def member$i: C$j = new C$j; def shared(x: Int) = C$j.value + x }
           |object C$i { val value = $i; def make: C$i = new C$i }
           |""".stripMargin, s"C$i.scala")
    }.toList
    compiler.newRun().compileSources(sources)
    compiler
  }

  private def errors(compiler: Compiler): List[String] =
    compiler.global.reporter.asInstanceOf[StoreReporter].infos.toList.collect {
      case i if i.severity.toString == "ERROR" => i.msg
    }

  @Test def rejectsTheCharStore(): Unit =
    assertEquals(List("-Yparallel-typer does not support -Yname-table:chars"), errors(compile("-Yname-table:chars", units = 2)))

  @Test def rejectsTheUnsharedStringStore(): Unit =
    assertEquals(List("-Yparallel-typer does not support -Yname-table:string"), errors(compile("-Yname-table:string", units = 2)))

  @Test def compilesOnTheSharedStore(): Unit = {
    val compiler = compile("-Yname-table:shared", units = 16)
    assertEquals(Nil, errors(compiler))
    import compiler.global._
    for (i <- 0 until 16) {
      val term = newTermName(s"C$i")
      assertTrue(term.toTypeName.toTermName eq term)
    }
    assertFalse(reporter.hasErrors)
  }
}
