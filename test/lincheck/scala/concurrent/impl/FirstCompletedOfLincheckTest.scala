package scala.concurrent.impl

import org.jetbrains.kotlinx.lincheck.annotations.{Operation, Param, Validate}
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.junit.Test

import scala.concurrent.ExecutionContext.parasitic
import scala.concurrent.Future

import LincheckSupport._

/** `Future.firstCompletedOf` over racing promises, including the deregistration of the handler from the losers
 *  (scala/bug#13058, see `FutureTest.t13058`).
 *
 *  As for `zipWith`, `first` is completed by a callback after the winning CAS, so it is not linearizable together
 *  with the inputs (an input may be observed completed before `first` is, and the input whose callback runs first
 *  wins, not the one whose CAS came first). The inputs are checked as write-once registers, `checkFirst` checks
 *  safety at any point, and `@Validate` checks the quiescent outcome:
 *   - `first` is completed iff some input is, with the value of one of the completed inputs;
 *   - once `first` is completed, the handler has been unregistered from every uncompleted input (state `Noop`);
 *     before that, each input holds exactly the one handler.
 */
class FirstCompletedOfLincheckTest {
  import FirstCompletedOfLincheckTest._
  private val ps = Array.fill(N)(new Promise.DefaultPromise[Int]())
  private val first = Future.firstCompletedOf(ps.toList.map(_.future))(parasitic)

  @Operation def complete(@Param(gen = classOf[IntGen], conf = "0:2") i: Int, @Param(gen = classOf[IntGen], conf = "1:3") v: Int): Boolean =
    ps(i).trySuccess(10 * i + v)

  @Operation def checkFirst(): Unit = {
    val f = first.value // read first: the inputs can only progress afterwards
    f.foreach(r => assert(ps.exists(_.value.contains(r)), s"first = $f, inputs = ${ps.map(_.value).mkString(", ")}"))
  }

  @Validate def validate(): Unit = {
    val inputs = ps.map(_.value)
    def state = s"first = ${first.value}, inputs = ${inputs.mkString(", ")}, callbacks = ${ps.map(numCallbacks).mkString(", ")}"
    first.value match {
      case None =>
        assert(inputs.forall(_.isEmpty), state)
        assert(ps.forall(numCallbacks(_) == 1), state)
      case Some(r) =>
        assert(inputs.contains(Some(r)), state)
        for (p <- ps if !p.isCompleted) assert(p.get() eq Noop, state)
    }
  }

  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[Spec]), getClass)
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking().sequentialSpecification(classOf[Spec]), getClass)
}

object FirstCompletedOfLincheckTest {
  final val N = 3

  class Spec {
    private val completed = new Array[Boolean](N)
    def complete(i: Int, v: Int): Boolean = !completed(i) && { completed(i) = true; true }
    def checkFirst(): Unit = ()
  }
}
