package scala.reflect.internal

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ScopeBenchmark {
  import scala.tools.nsc._

  @Param(Array("1", "8", "32", "64", "256"))
  var size: Int = _

  val g: Global = {
    val settings = new Settings()
    settings.usejavacp.value = true
    new Global(settings)
  }
  var syms: Array[g.Symbol] = _
  var cached: g.Scope = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    new g.Run()
    syms = Array.tabulate[g.Symbol](size)(i => g.NoSymbol.newValue(g.TermName("x" + i)))
    cached = g.newScopeWith(syms.toIndexedSeq: _*)
    cached.toList
  }

  private def build(): g.Scope = {
    val scope = g.newScope
    var i = 0
    while (i < syms.length) {
      scope.enter(syms(i))
      i += 1
    }
    scope
  }

  /** Build a scope and iterate it once, as `Flatten` does. */
  @Benchmark def buildForeach(bh: Blackhole): Unit =
    build().foreach(bh.consume(_))

  /** As `buildForeach`, with the old implementation of `Scope.foreach`. */
  @Benchmark def buildToListForeach(bh: Blackhole): Unit =
    build().toList.foreach(bh.consume(_))

  /** Build a scope and iterate it twice. */
  @Benchmark def buildForeachTwice(bh: Blackhole): Unit = {
    val scope = build()
    scope.foreach(bh.consume(_))
    scope.foreach(bh.consume(_))
  }

  /** Just build the scope, to subtract from the above. */
  @Benchmark def buildOnly(bh: Blackhole): Unit =
    bh.consume(build())

  /** Iterate a scope whose `toList` is already cached. */
  @Benchmark def cachedForeach(bh: Blackhole): Unit =
    cached.foreach(bh.consume(_))
}
