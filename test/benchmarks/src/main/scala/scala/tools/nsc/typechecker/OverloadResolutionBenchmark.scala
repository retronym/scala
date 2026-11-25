package scala.tools.nsc.typechecker

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** Time to type check `n` applications of overloaded methods with polymorphic alternatives, the
 *  case where `Infer.isApplicable` used to clone the alternative's type params for every check.
 *  One `Global` is reused across compiles, so an op is a new `Run` through `typer`.
 *
 *  - `arrayApply`: `Array.apply`, whose ten alternatives include a polymorphic one.
 *  - `poly`: two polymorphic alternatives of the same arity, with function literal arguments.
 *  - `dependent`: as `poly`, but a type param's bound mentions another, so the type params are
 *    still cloned; it shows the cost of the check alone.
 *
 *  {{{
 *  bench/Jmh/run OverloadResolutionBenchmark
 *  }}}
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(value = 2, jvmArgsAppend = Array("-Xms2G", "-Xmx2G", "-Xss8m"))
@Threads(1)
@Warmup(iterations = 30, time = 1)
@Measurement(iterations = 15, time = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class OverloadResolutionBenchmark {
  @Param(Array("arrayApply", "poly", "dependent")) var shape: String = _
  @Param(Array("256")) var n: Int = _

  private var global: Global = _
  private var reporter: StoreReporter = _
  private var code: String = _

  @Setup(Level.Trial) def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.processArguments(List("-Ystop-after:typer"), processAll = true)
    reporter = new StoreReporter(settings)
    global = new Global(settings, reporter)
    code = OverloadResolutionBenchmark.source(shape, n)
  }

  @Benchmark def typecheck(): Unit = {
    reporter.reset()
    val g = global
    val run = new g.Run
    run.compileSources(new BatchSourceFile("Overloads.scala", code) :: Nil)
    if (reporter.hasErrors) throw new IllegalStateException(reporter.infos.mkString("compilation failed:\n", "\n", ""))
  }
}

object OverloadResolutionBenchmark {
  def source(shape: String, n: Int): String = {
    val (api, call) = shape match {
      case "arrayApply" =>
        ("", (i: Int) => s"""Array(i + $i, i); Array("$i", s)""")
      case "poly" =>
        ("""object Api {
           |  def run[A](f: Int => A, n: Int): A = f(n)
           |  def run[A, B](f: A => B, a: A): B = f(a)
           |}""".stripMargin,
         (i: Int) => s"""Api.run((x: Int) => x + $i, i); Api.run((x: String) => x.length + $i, s)""")
      case "dependent" =>
        ("""object Api {
           |  def run[A](f: Int => A, n: Int): A = f(n)
           |  def run[A, B <: Seq[A]](f: A => B, a: A): B = f(a)
           |}""".stripMargin,
         (i: Int) => s"""Api.run((x: Int) => x + $i, i); Api.run((x: String) => List(x), s)""")
    }
    val calls = (0 until n).map(i => s"    ${call(i)}").mkString("\n")
    s"""$api
       |class Calls {
       |  def calls(i: Int, s: String): Unit = {
       |$calls
       |  }
       |}
       |""".stripMargin
  }
}
