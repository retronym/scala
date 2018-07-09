package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorBenchmark {
  @Param(Array("10", "100", "1000"))
  var size: Int = _

  var values0: Vector[Any] = _
  var values1: Vector[Any] = _

  @Setup(Level.Trial) def init(): Unit = {
    def mkVector = (0 to size).map((i: Int) => "value_" + i).toVector
    values0 = mkVector
    values1 = mkVector
  }

  @Benchmark def equals: Boolean = values1.equals(values0)
}
