package scala.concurrent.impl

import org.jetbrains.kotlinx.lincheck.annotations.{Operation, Param}
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.junit.Test

import LincheckSupport._

/** `DefaultPromise` as a linearizable write-once register: exactly one completer wins, and
 *  `value`/`isCompleted` never go backwards. */
class DefaultPromiseLincheckTest {
  private val p = new Promise.DefaultPromise[Int]()

  @Operation def tryComplete(@Param(gen = classOf[IntGen], conf = "1:3") v: Int): Boolean = p.trySuccess(v)
  @Operation def tryFail(@Param(gen = classOf[IntGen], conf = "1:3") code: Int): Boolean = p.tryFailure(new TestException(code))
  @Operation def value(): Option[Either[Int, Int]] = encode(p.value)
  @Operation def isCompleted(): Boolean = p.isCompleted

  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[DefaultPromiseLincheckTest.Spec]), getClass)
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking().sequentialSpecification(classOf[DefaultPromiseLincheckTest.Spec]), getClass)
}

object DefaultPromiseLincheckTest {
  class Spec {
    private var state: Option[Either[Int, Int]] = None
    private def set(v: Either[Int, Int]): Boolean = state.isEmpty && { state = Some(v); true }
    def tryComplete(v: Int): Boolean = set(Right(v))
    def tryFail(code: Int): Boolean = set(Left(code))
    def value(): Option[Either[Int, Int]] = state
    def isCompleted(): Boolean = state.isDefined
  }
}
