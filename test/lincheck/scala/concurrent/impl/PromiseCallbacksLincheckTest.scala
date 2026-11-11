package scala.concurrent.impl

import java.util.concurrent.atomic.AtomicIntegerArray

import org.jetbrains.kotlinx.lincheck.annotations.{Operation, Param, Validate}
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.junit.Test

import scala.concurrent.ExecutionContext.parasitic
import scala.util.Success

import LincheckSupport._

/** Callback registration racing with completion and with unregistration of other callbacks.
 *
 *  Callbacks run on `parasitic`, so by the end of an invocation (when `@Validate` runs) every callback that will
 *  ever fire has fired. Invariants:
 *   - a callback registered with `onComplete` fires exactly once with the promise's value if the promise is
 *     completed (no lost registrations, no double dispatch), and not at all otherwise;
 *   - a callback registered and then immediately unregistered via `onCompleteWithUnregister` fires at most once:
 *     never if the unregistration took effect before completion, once (possibly concurrently with, or even after,
 *     the unregistration returning) if completion won the race. Unregistration is a GC aid, not a cancellation
 *     guarantee: `unregisterCallback` is a no-op on a completed promise, and completion dispatches the callbacks it
 *     swapped out of the state without re-checking them;
 *   - unregistering one callback never removes (or fires) a concurrently registered other callback.
 *
 *  `trySuccess` is checked for linearizability (exactly one winner); the callback ops have no result.
 */
class PromiseCallbacksLincheckTest {
  import PromiseCallbacksLincheckTest._
  private val p = new Promise.DefaultPromise[Int]()
  // per id: registrations / firings, for plain and for unregistered callbacks
  private val registered        = new AtomicIntegerArray(Ids)
  private val fired             = new AtomicIntegerArray(Ids)
  private val firedWrongValue   = new AtomicIntegerArray(Ids)
  private val registeredUnreg   = new AtomicIntegerArray(Ids)
  private val firedUnreg        = new AtomicIntegerArray(Ids)

  @Operation def trySuccess(@Param(gen = classOf[IntGen], conf = "1:3") v: Int): Boolean = p.trySuccess(v)

  @Operation def onComplete(@Param(gen = classOf[IntGen], conf = "0:2") id: Int): Unit = {
    registered.incrementAndGet(id)
    p.onComplete { r =>
      fired.incrementAndGet(id)
      if (!p.value.contains(r)) firedWrongValue.incrementAndGet(id)
    }(parasitic)
  }

  @Operation def registerAndUnregister(@Param(gen = classOf[IntGen], conf = "0:2") id: Int): Unit = {
    registeredUnreg.incrementAndGet(id)
    val unregister = p.onCompleteWithUnregister(_ => firedUnreg.incrementAndGet(id))(parasitic)
    unregister()
  }

  @Validate def validate(): Unit = {
    val completed = p.isCompleted
    for (id <- 0 until Ids) {
      val expected = if (completed) registered.get(id) else 0
      assert(fired.get(id) == expected, s"id $id: ${fired.get(id)} firings for ${registered.get(id)} registrations, completed=$completed")
      assert(firedWrongValue.get(id) == 0, s"id $id: callback saw a value other than ${p.value}")
      assert(firedUnreg.get(id) <= registeredUnreg.get(id), s"id $id: unregistered callback fired ${firedUnreg.get(id)} times for ${registeredUnreg.get(id)} registrations")
      if (!completed) assert(firedUnreg.get(id) == 0, s"id $id: unregistered callback fired on an incomplete promise")
    }
    // all callbacks have been dispatched (and the state no longer refers to them) once completed
    if (completed) assert(p.get().isInstanceOf[Success[_]], p.get())
    // unregistered callbacks don't linger: at most the plain `onComplete` ones remain
    else {
      val total = (0 until Ids).map(registered.get).sum
      assert(numCallbacks(p) == total, s"${numCallbacks(p)} callbacks registered, expected $total")
    }
  }

  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[Spec]), getClass)
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking().sequentialSpecification(classOf[Spec]), getClass)
}

object PromiseCallbacksLincheckTest {
  final val Ids = 3

  class Spec {
    private var completed = false
    def trySuccess(v: Int): Boolean = !completed && { completed = true; true }
    def onComplete(id: Int): Unit = ()
    def registerAndUnregister(id: Int): Unit = ()
  }
}
