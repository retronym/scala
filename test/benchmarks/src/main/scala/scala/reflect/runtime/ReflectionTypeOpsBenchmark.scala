package scala.reflect.runtime

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

object ReflectionTypeOpsBenchmark {
  trait Base[+A] { def head: A; def pair[B >: A](b: B): (A, B) }
  trait Mid[+A, B] extends Base[A] { def mid: Map[B, A]; def fold[C](z: C)(f: (C, A) => C): C }
  trait Side[-A] { def accept(a: A): Unit }
  abstract class Leaf[A] extends Mid[List[A], Int] with Side[Seq[A]] with Serializable
  abstract class Other[A] extends Mid[List[A], Int] with Serializable
}

/** Type operations of runtime reflection over a shared, fully initialised universe.
 *
 *  Run with `-t 1` and `-t 4` to see the cost of per-thread state: after setup the
 *  symbols are complete, so the operations run outside the GIL and contend only on
 *  whatever per-thread storage the universe uses.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ReflectionTypeOpsBenchmark {
  import ReflectionTypeOpsBenchmark._
  import scala.reflect.runtime.universe._

  var leafString: Type = _
  var baseSeqAny: Type = _
  var baseSeqInt: Type = _
  var otherInt: Type = _
  var midDecls: List[Symbol] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    leafString = typeOf[Leaf[String]]
    baseSeqAny = typeOf[Base[Seq[Any]]]
    baseSeqInt = typeOf[Base[Seq[Int]]]
    otherInt = typeOf[Other[Int]]
    midDecls = typeOf[Mid[_, _]].typeSymbol.info.decls.toList ++ typeOf[Base[_]].typeSymbol.info.decls.toList
    // complete everything the benchmarks touch, so that they run without the GIL
    subtypeTrue(); subtypeFalse(); lubLeafOther(); asSeenFrom(null); members(null)
  }

  @Benchmark def subtypeTrue(): Boolean = leafString <:< baseSeqAny

  @Benchmark def subtypeFalse(): Boolean = leafString <:< baseSeqInt

  @Benchmark def lubLeafOther(): Type = lub(leafString :: otherInt :: Nil)

  @Benchmark def asSeenFrom(bh: Blackhole): Unit = {
    var ds = midDecls
    while (ds.nonEmpty) {
      val sig = ds.head.typeSignatureIn(leafString)
      if (bh ne null) bh.consume(sig)
      ds = ds.tail
    }
  }

  @Benchmark def members(bh: Blackhole): Unit = {
    val ms = otherInt.members
    if (bh ne null) bh.consume(ms.size)
  }
}
