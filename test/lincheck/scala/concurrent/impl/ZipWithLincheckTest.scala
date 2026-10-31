package scala.concurrent.impl

import org.jetbrains.kotlinx.lincheck.annotations.{Operation, Param, Validate}
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.junit.Test

import scala.concurrent.ExecutionContext.parasitic
import scala.util.{Failure, Success}

import LincheckSupport._

/** `zipWith` of two racing promises (the uncompleted-at-zip-time path, with its `getAndSet` rendezvous buffer).
 *
 *  `zipped` is not linearizable together with its inputs, by design: it is completed by callbacks that run after
 *  the input's CAS, so another thread may observe an input completed while `zipped` is not yet, and when both inputs
 *  fail, either failure may win regardless of which input CAS came first. So the inputs are checked for
 *  linearizability (two write-once registers), `checkZipped` checks safety at any point (`zipped` only ever holds a
 *  value derivable from the inputs' current values), and `@Validate` checks the quiescent outcome:
 *   - both succeeded: `Success(v1 + v2)`;
 *   - exactly one failed: that failure;
 *   - both failed: one of the two failures;
 *   - otherwise: not completed.
 */
class ZipWithLincheckTest {
  private val p1 = new Promise.DefaultPromise[Int]()
  private val p2 = new Promise.DefaultPromise[Int]()
  private val zipped = p1.future.zipWith(p2.future)(_ + _)(parasitic)

  @Operation def complete1(@Param(gen = classOf[IntGen], conf = "1:2") v: Int): Boolean = p1.trySuccess(v)
  @Operation def complete2(@Param(gen = classOf[IntGen], conf = "10:20") v: Int): Boolean = p2.trySuccess(v)
  @Operation def fail1(@Param(gen = classOf[IntGen], conf = "1:2") code: Int): Boolean = p1.tryFailure(new TestException(code))
  @Operation def fail2(@Param(gen = classOf[IntGen], conf = "10:20") code: Int): Boolean = p2.tryFailure(new TestException(code))

  @Operation def checkZipped(): Unit = {
    val z = encode(zipped.value) // read first: the inputs can only progress afterwards
    val (v1, v2) = (encode(p1.value), encode(p2.value))
    z match {
      case None =>
      case Some(Right(s)) => assert(v1.exists(_.isRight) && v2.exists(_.isRight) && v1.get.toOption.get + v2.get.toOption.get == s, (z, v1, v2))
      case Some(Left(c))  => assert(v1.contains(Left(c)) || v2.contains(Left(c)), (z, v1, v2))
    }
  }

  @Validate def validate(): Unit = {
    val z = encode(zipped.value)
    val expected: Set[Option[Either[Int, Int]]] = (p1.value, p2.value) match {
      case (Some(Success(a)), Some(Success(b)))          => Set(Some(Right(a + b)))
      case (Some(f1 @ Failure(_)), Some(f2 @ Failure(_))) => Set(Some(encode(f1)), Some(encode(f2)))
      case (Some(f @ Failure(_)), _)                       => Set(Some(encode(f)))
      case (_, Some(f @ Failure(_)))                       => Set(Some(encode(f)))
      case _                                               => Set(None)
    }
    assert(expected(z), s"zipped = $z, p1 = ${p1.value}, p2 = ${p2.value}")
  }

  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[ZipWithLincheckTest.Spec]), getClass)
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking().sequentialSpecification(classOf[ZipWithLincheckTest.Spec]), getClass)
}

object ZipWithLincheckTest {
  class Spec {
    private var c1, c2 = false
    def complete1(v: Int): Boolean = !c1 && { c1 = true; true }
    def complete2(v: Int): Boolean = !c2 && { c2 = true; true }
    def fail1(code: Int): Boolean = complete1(code)
    def fail2(code: Int): Boolean = complete2(code)
    def checkZipped(): Unit = ()
  }
}
