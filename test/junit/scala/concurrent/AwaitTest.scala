package scala.concurrent

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.junit.Assert._
import org.junit.{After, Before, Test}

import scala.concurrent.duration._
import scala.tools.testkit.AssertUtil.{assertThrows, waitForIt}
import scala.util.{Failure, Success, Try}

class AwaitTest {
  import AwaitTest.CountingBlockContext
  @Before def before(): Unit = Thread.interrupted()
  @After def after(): Unit = Thread.interrupted()

  /** Runs `body` on a fresh thread, interrupts that thread once it is parked, and returns the
   *  outcome of `body` along with the thread's interrupt status at the end of `body`. */
  private def interruptWhileBlocked[A](body: => A): (Try[A], Boolean) = {
    val outcome = new AtomicReference[(Try[A], Boolean)]
    val t = new Thread(() => {
      val r = try Success(body) catch { case t: Throwable => Failure(t) }
      outcome.set((r, Thread.currentThread.isInterrupted))
    })
    t.setDaemon(true)
    t.start()
    waitForIt(t.getState == Thread.State.WAITING || t.getState == Thread.State.TIMED_WAITING || !t.isAlive, label = "blocked")
    t.interrupt()
    t.join(30000)
    assertFalse("thread did not terminate", t.isAlive)
    outcome.get
  }

  private def assertInterrupted(o: (Try[_], Boolean)): Unit = {
    o match {
      case (Failure(_: InterruptedException), interrupted) =>
        assertFalse("interrupt status should be cleared after throwing InterruptedException", interrupted)
      case other => fail(s"Expected InterruptedException, was $other")
    }
  }

  @Test def interruptedAwaitOnPromise(): Unit =
    for (atMost <- List[Duration](Duration.Inf, 30.seconds); ready <- List(true, false)) {
      val p = Promise[Int]()
      assertInterrupted(interruptWhileBlocked(if (ready) Await.ready(p.future, atMost) else Await.result(p.future, atMost)))
      assertFalse(p.isCompleted)
      // the future is still usable: a subsequent await succeeds once completed
      val waiter = new Thread(() => p.success(42))
      waiter.start()
      assertEquals(42, Await.result(p.future, 30.seconds))
      waiter.join(30000)
      assertTrue(p.trySuccess(1) == false)
    }

  @Test def awaitAfterInterruptedAwaitOnSameThread(): Unit = {
    val p = Promise[Int]()
    val gotIE = new CountDownLatch(1)
    val second = new AtomicReference[Try[Int]]
    val t = new Thread(() => {
      try Await.result(p.future, Duration.Inf)
      catch { case _: InterruptedException => gotIE.countDown() }
      second.set(Try(Await.result(p.future, 30.seconds)))
    })
    t.start()
    waitForIt(t.getState == Thread.State.WAITING, label = "blocked")
    t.interrupt()
    assertTrue(gotIE.await(30, TimeUnit.SECONDS))
    assertFalse(p.isCompleted)
    p.success(7)
    t.join(30000)
    assertEquals(Success(7), second.get)
  }

  @Test def preInterruptedAwaitOnIncompleteFutureThrows(): Unit =
    for (atMost <- List[Duration](Duration.Inf, 1.second)) {
      val p = Promise[Int]()
      Thread.currentThread.interrupt()
      assertThrows[InterruptedException](Await.ready(p.future, atMost))
      assertFalse("interrupt status should be cleared", Thread.interrupted())
      Thread.currentThread.interrupt()
      assertThrows[InterruptedException](Await.result(p.future, atMost))
      assertFalse("interrupt status should be cleared", Thread.interrupted())
      assertFalse(p.isCompleted)
    }

  @Test def preInterruptedAwaitOnCompletedFutureReturnsAndKeepsInterruptStatus(): Unit = {
    Thread.currentThread.interrupt()
    assertEquals(1, Await.result(Future.successful(1), 1.second))
    assertTrue(Thread.interrupted())
  }

  @Test def preInterruptedAwaitWithNonPositiveTimeoutTimesOutAndKeepsInterruptStatus(): Unit = {
    // no blocking is attempted, so the interrupt is not observed
    Thread.currentThread.interrupt()
    assertThrows[TimeoutException](Await.result(Promise[Int]().future, Duration.Zero))
    assertTrue(Thread.interrupted())
  }

  @Test def awaitIncompleteTimesOut(): Unit = {
    val p = Promise[Int]()
    assertThrows[TimeoutException](Await.result(p.future, 10.millis), _ == "Future timed out after [10 milliseconds]")
    assertThrows[TimeoutException](Await.ready(p.future, 10.millis))
    for (d <- List[Duration](Duration.Zero, -1.second, Duration.MinusInf))
      assertThrows[TimeoutException](Await.result(p.future, d))
    assertThrows[IllegalArgumentException](Await.result(p.future, Duration.Undefined))
    assertThrows[IllegalArgumentException](Await.ready(p.future, Duration.Undefined))
    p.success(1)
    assertEquals(1, Await.result(p.future, 10.millis))
  }

  @Test def awaitCompletedFailureThrowsUnboxed(): Unit = {
    val e = new IllegalStateException
    assertThrows[IllegalStateException](Await.result(Future.failed(e), 1.second), _ eq e.getMessage)
    val f = Future.failed[Int](e)
    assertSame(f, Await.ready(f, 1.second))
    // a fatal failure is boxed by the Promise, so Await rethrows the box
    assertThrows[ExecutionException](Await.result(Future.failed(new InterruptedException), 1.second))
  }

  @Test def awaitCompletedFutureDoesNotCallBlocking(): Unit = {
    val counted = new CountingBlockContext
    BlockContext.withBlockContext(counted) {
      Await.result(Future.successful(1), 1.second)
      Await.ready(Future.successful(1), Duration.Inf)
    }
    assertEquals(0, counted.count.get)
    BlockContext.withBlockContext(counted) {
      assertThrows[TimeoutException](Await.result(Promise[Int]().future, 1.milli))
      assertThrows[TimeoutException](Await.ready(Promise[Int]().future, 1.milli))
    }
    assertEquals(2, counted.count.get)
  }

  @Test def awaitNonFutureAwaitableUsesBlocking(): Unit = {
    val counted = new CountingBlockContext
    val a = new Awaitable[Int] {
      def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this
      def result(atMost: Duration)(implicit permit: CanAwait): Int = 5
    }
    BlockContext.withBlockContext(counted) {
      assertEquals(5, Await.result(a, Duration.Inf))
      assertSame(a, Await.ready(a, Duration.Inf))
    }
    assertEquals(2, counted.count.get)
  }

  // ---- Future.never ----

  @Test def neverInterrupted(): Unit = {
    assertInterrupted(interruptWhileBlocked(Await.ready(Future.never, Duration.Inf)))
    assertInterrupted(interruptWhileBlocked(Await.ready(Future.never, 30.seconds)))
    assertInterrupted(interruptWhileBlocked(Await.result(Future.never, Duration.Inf)))
    assertInterrupted(interruptWhileBlocked(Await.result(Future.never, 30.seconds)))
  }

  @Test def neverPreInterrupted(): Unit =
    for (d <- List[Duration](Duration.Inf, 30.seconds)) {
      Thread.currentThread.interrupt()
      assertThrows[InterruptedException](Await.ready(Future.never, d))
      assertFalse(Thread.interrupted())
    }

  @Test def neverTimesOut(): Unit = {
    assertThrows[TimeoutException](Await.ready(Future.never, 10.millis))
    assertThrows[TimeoutException](Await.result(Future.never, 10.millis))
    for (d <- List[Duration](Duration.MinusInf, Duration.Zero, -1.second)) {
      assertThrows[TimeoutException](Await.result(Future.never, d))
      assertThrows[TimeoutException](Await.ready(Future.never, d))
    }
    assertThrows[IllegalArgumentException](Await.result(Future.never, Duration.Undefined))
    assertThrows[IllegalArgumentException](Await.ready(Future.never, Duration.Undefined))
  }

  @Test def neverTimeoutRespectsDeadline(): Unit = {
    val start = System.nanoTime()
    assertThrows[TimeoutException](Await.ready(Future.never, 50.millis))
    assertTrue((System.nanoTime() - start).nanos >= 50.millis)
  }
}

object AwaitTest {
  final class CountingBlockContext extends BlockContext {
    val count = new AtomicInteger
    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = { count.incrementAndGet(); thunk }
  }
}
