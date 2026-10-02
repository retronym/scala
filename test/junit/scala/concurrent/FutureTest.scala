
package scala.concurrent

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.tools.testkit.AssertUtil._
import scala.util.{Success, Try}
import duration.Duration, Duration.{Inf, Undefined}
import duration.DurationInt
import scala.collection.mutable.ListBuffer
import scala.concurrent.impl.Promise.DefaultPromise
import scala.util.chaining._

class FutureTest {
  @Test
  def testZipWithFailFastBothWays(): Unit = {
    import ExecutionContext.Implicits.global

    val p1 = Promise[Int]()
    val p2 = Promise[Int]()

    // Make sure that the combined future fails early, after the earlier failure occurs, and does not
    // wait for the later failure regardless of which one is on the left and which is on the right
    p1.failure(new Exception("Boom Early"))
    val f1 = p1.future
    val f2 = p2.future

    val scala.util.Failure(fa) = Try(Await.result(f1.zip(f2), Inf))
    val scala.util.Failure(fb) = Try(Await.result(f2.zip(f1), Inf))

    val scala.util.Failure(fc) = Try(Await.result(f1.zipWith(f2)((_, _)), Inf))
    val scala.util.Failure(fd) = Try(Await.result(f2.zipWith(f1)((_, _)), Inf))

    val scala.util.Failure(fe) = Try(Await.result(Future.sequence(Seq(f1, f2)), Inf))
    val scala.util.Failure(ff) = Try(Await.result(Future.sequence(Seq(f2, f1)), Inf))

    val scala.util.Failure(fg) = Try(Await.result(Future.traverse(Seq(0, 1))(Seq(f1, f2)(_)), Inf))
    val scala.util.Failure(fh) = Try(Await.result(Future.traverse(Seq(0, 1))(Seq(f1, f2)(_)), Inf))

    // Make sure the early failure is always reported, regardless of whether it's on
    // the left or right of the zip/zipWith/sequence/traverse
    assert(fa.getMessage == "Boom Early")
    assert(fb.getMessage == "Boom Early")
    assert(fc.getMessage == "Boom Early")
    assert(fd.getMessage == "Boom Early")
    assert(fe.getMessage == "Boom Early")
    assert(ff.getMessage == "Boom Early")
    assert(fg.getMessage == "Boom Early")
    assert(fh.getMessage == "Boom Early")
  }

  @Test
  def `bug/issues#10513 firstCompletedOf must not leak references`(): Unit = {
    val unfulfilled = Promise[AnyRef]()
    val quick       = Promise[AnyRef]()
    val result      = new AnyRef
    // all callbacks will be registered
    val first = Future.firstCompletedOf(List(quick.future, unfulfilled.future))(ExecutionContext.parasitic)
    // callbacks run parasitically to avoid race or waiting for first future;
    // normally we have no guarantee that firstCompletedOf completed, so we assert that this assumption held
    assertNotReachable(result, unfulfilled) {
      quick.complete(Try(result))
      assertTrue("First must complete", first.isCompleted)
    }
    /* The test has this structure under the hood:
    val p = Promise[String]
    val q = Promise[String]
    val res = Promise[String]
    val s = "hi"
    p.future.onComplete(t => res.complete(t))
    q.future.onComplete(t => res.complete(t))   // previously, uncompleted promise held reference to promise completed with value
    assertNotReachable(s, q) {
      p.complete(Try(s))
    }
    */
  }

  @Test
  def `bug/issues#9304 blocking shouldn't prevent Future from being resolved`(): Unit = {
    implicit val directExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(_.run())

    val p = Promise[Int]()
    val p0 = Promise[Int]()
    val p1 = Promise[Int]()

    val f = p0.future
      .flatMap { _ =>
        p.future
          .flatMap { _ =>
            val f = p0.future.flatMap { _ =>
              Future.successful(1)
            }
            // At this point scala.concurrent.Future.InternalCallbackExecutor has 1 runnable in _tasksLocal
            // (flatMap from the previous line)

            // blocking sets _tasksLocal to Nil (instead of null). Next it calls Batch.run, which checks
            // that _tasksLocal must be null, throws exception and all tasks are lost.
            // ... Because blocking throws an exception, we need to swallow it to demonstrate that Future `f` is not
            // completed.
            Try(blocking {
              1
            })

            f
          }
      }

    p.completeWith(p1.future.map(_ + 1))
    p0.complete(Success(0))
    p1.complete(Success(1))

    assertTrue(p.future.isCompleted)
    assertEquals(Some(Success(2)), p.future.value)

    assertTrue(f.isCompleted)
    assertEquals(Some(Success(1)), f.value)
  }

  private val Noop = impl.Promise.getClass.getDeclaredFields.find(_.getName.contains("Noop")).get.tap(_.setAccessible(true)).get(impl.Promise)

  private def numTransforms(p: AnyRef) = { // a DefaultPromise, as Promise or Future
    def count(cs: impl.Promise.Callbacks[_]): Int = cs match {
      case Noop => 0
      case m: impl.Promise.ManyCallbacks[_] => 1 + count(m.rest)
      case _ => 1
    }
    val cs = p.asInstanceOf[DefaultPromise[_]].get().asInstanceOf[impl.Promise.Callbacks[_]]
    count(cs)
  }

  @Test def t13058(): Unit = {
    implicit val directExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(_.run())

    locally {
      val p1 = Promise[Int]()
      val p2 = Promise[Int]()
      val p3 = Promise[Int]()

      p3.future.onComplete(_ => ())
      p3.future.onComplete(_ => ())

      assert(p2.asInstanceOf[DefaultPromise[_]].get() eq Noop)
      assert(numTransforms(p3) == 2)
      val ops3 = p3.asInstanceOf[DefaultPromise[_]].get()

      val first = Future.firstCompletedOf(List(p1.future, p2.future, p3.future))

      assert(numTransforms(p1) == 1)
      assert(numTransforms(p2) == 1)
      assert(numTransforms(p3) == 3)

      val succ = Success(42)
      p1.complete(succ)
      assert(Await.result(first, Inf) == 42)

      assert(p1.asInstanceOf[DefaultPromise[_]].get() eq succ)
      assert(p2.asInstanceOf[DefaultPromise[_]].get() eq Noop)
      assert(p3.asInstanceOf[DefaultPromise[_]].get() eq ops3)

      assert(numTransforms(p2) == 0)
      assert(numTransforms(p3) == 2)
    }

    locally {
      val b = ListBuffer.empty[String]
      var p = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      assert(p.get() eq Noop)
      val a1 = p.onCompleteWithUnregister(_ => ())
      a1()
      assert(p.get() eq Noop)

      val a2 = p.onCompleteWithUnregister(_ => b += "a2")
      p.onCompleteWithUnregister(_ => b += "b2")
      a2()
      assert(numTransforms(p) == 1)
      p.complete(Success(41))
      assert(b.mkString == "b2")

      p = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      b.clear()
      p.onCompleteWithUnregister(_ => b += "a3")
      val b3 = p.onCompleteWithUnregister(_ => b += "b3")
      p.onCompleteWithUnregister(_ => b += "c3")
      b3()
      assert(numTransforms(p) == 2)
      p.complete(Success(41))
      assert(b.mkString == "a3c3")


      p = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      b.clear()
      p.onCompleteWithUnregister(_ => b += "a4")
      p.onCompleteWithUnregister(_ => b += "b4")
      val c4 = p.onCompleteWithUnregister(_ => b += "c4")
      c4()
      assert(numTransforms(p) == 2)
      p.complete(Success(41))
      println(b.mkString)
      assert(b.mkString == "b4a4")
    }
  }

  @Test def t13197(): Unit = {
    val p = Promise[Int]()
    p.future.onComplete(_ => ())(ExecutionContext.parasitic) // callback that should not be removed
    val ops = p.asInstanceOf[DefaultPromise[_]].get()

    for (_ <- 1 to 100) {
      assertThrows[TimeoutException](Await.result(p.future, 1.nanosecond))
      assertThrows[TimeoutException](Await.ready(p.future, 1.nanosecond))
    }
    assertEquals(1, numTransforms(p)) // only the one `onComplete` callback, no leftovers from the Await calls
    assertTrue(p.asInstanceOf[DefaultPromise[_]].get() eq ops)

    Thread.currentThread.interrupt()
    assertThrows[InterruptedException](Await.result(p.future, Inf)) // latch is added, interrupt flag => exception, latch removed
    Thread.currentThread.interrupt()
    assertThrows[InterruptedException](Await.result(p.future, 1.hour))
    assertEquals(1, numTransforms(p)) // no leftovers from interrupted Await calls

    p.success(42)
    assertEquals(42, Await.result(p.future, 1.nanosecond))
  }

  // Completing `gate` links `inner` to `outer`, moving inner's callbacks to outer
  private def linked(inner: Future[Int]) = {
    val gate = Promise[Unit]()
    val outer = gate.future.flatMap(_ => inner)(ExecutionContext.parasitic)
    (gate, outer)
  }

  private def isLinked(p: AnyRef) = p.asInstanceOf[DefaultPromise[_]].get().isInstanceOf[impl.Promise.Link[_]]

  private def spinUntil(cond: => Boolean): Unit = {
    val deadline = System.nanoTime + 10.seconds.toNanos
    while (!cond) {
      if (System.nanoTime - deadline > 0) throw new AssertionError("condition not reached in time")
      Thread.`yield`()
    }
  }

  // Runs `body` on a new thread; the returned function joins it and rethrows any failure
  private def fork(body: => Unit): () => Unit = {
    @volatile var failure: Throwable = null
    val t = new Thread(() => try body catch { case e: Throwable => failure = e })
    t.start()
    () => { t.join(); if (failure ne null) throw failure }
  }

  // Await `f` on the current thread, `during` runs on another thread once the latch is registered
  // on `registeredOn`, and is responsible for interrupting the waiting thread.
  private def interruptedAwait(f: Future[Int], registeredOn: AnyRef, baseline: Int, atMost: Duration)(during: Thread => Unit): Unit = {
    val waiter = Thread.currentThread
    val join = fork {
      spinUntil(numTransforms(registeredOn) == baseline + 1)
      during(waiter)
    }
    try assertThrows[InterruptedException](Await.ready(f, atMost))
    finally {
      join()
      Thread.interrupted() // in case `during` failed before the waiter consumed the interrupt
    }
  }

  @Test def t13197Linked(): Unit = {
    // Await: already linked
    locally {
      val inner = Promise[Int]()
      val (gate, outer) = linked(inner.future)
      gate.success(())
      assertTrue(isLinked(inner))
      for (_ <- 1 to 100) {
        assertThrows[TimeoutException](Await.ready(inner.future, 1.nanosecond))
        assertThrows[TimeoutException](Await.result(inner.future, 1.nanosecond))
      }
      assertEquals(0, numTransforms(outer))
    }

    // firstCompletedOf: linked after adding the handler
    locally {
      val inner = Promise[Int]()
      val (gate, outer) = linked(inner.future)
      val other = Promise[Int]()
      val first = Future.firstCompletedOf(List(inner.future, other.future))(ExecutionContext.parasitic)
      gate.success(())
      assertEquals(1, numTransforms(outer))
      other.success(1)
      assertEquals(1, Await.result(first, Inf))
      assertEquals(0, numTransforms(outer))
    }

    // firstCompletedOf with a chain of links: a -> b -> c
    locally {
      val a = Promise[Int]()
      val (g1, b) = linked(a.future)
      val (g2, c) = linked(b)
      g1.success(()); g2.success(())
      assertTrue(isLinked(a))
      assertTrue(isLinked(b))
      val other = Promise[Int]()
      val first = Future.firstCompletedOf(List(a.future, other.future))(ExecutionContext.parasitic)
      assertEquals(1, numTransforms(c))
      other.success(1)
      assertEquals(1, Await.result(first, Inf))
      assertEquals(0, numTransforms(c))
    }
  }

  // The latch is registered on `inner`, then moved to `outer` by linking while the waiter blocks.
  // The interrupt is only delivered after linking, so the removal must follow the `Link`.
  @Test def t13197LinkedWhileWaiting(): Unit = for (atMost <- List(Inf, 1.hour)) {
    val inner = Promise[Int]()
    val (gate, outer) = linked(inner.future)
    interruptedAwait(inner.future, registeredOn = inner, baseline = 0, atMost) { waiter =>
      gate.success(())
      assertTrue(isLinked(inner))
      assertEquals(1, numTransforms(outer)) // the latch moved
      waiter.interrupt()
    }
    assertEquals(0, numTransforms(outer))
  }

  // As above, with a chain a -> b -> c formed while waiting
  @Test def t13197LinkChainWhileWaiting(): Unit = {
    val a = Promise[Int]()
    val (g1, b) = linked(a.future)
    val (g2, c) = linked(b)
    interruptedAwait(a.future, registeredOn = a, baseline = 0, Inf) { waiter =>
      g1.success(())
      g2.success(())
      assertTrue(isLinked(a))
      assertTrue(isLinked(b))
      assertEquals(1, numTransforms(c))
      waiter.interrupt()
    }
    assertEquals(0, numTransforms(c))
  }

  // The root of the link is completed before the waiter wakes up: nothing to unregister, and the value is visible
  @Test def t13197LinkedRootCompletedWhileWaiting(): Unit = {
    val inner = Promise[Int]()
    val (gate, outer) = linked(inner.future)
    val join = fork {
      spinUntil(numTransforms(inner) == 1)
      gate.success(())
      inner.success(42) // completes `outer`, the root
    }
    assertEquals(42, Await.result(inner.future, Inf))
    join()
    assertEquals(Some(Success(42)), outer.value)
    assertEquals(Some(Success(42)), inner.future.value)
  }

  // Only the latch is removed; callbacks registered before and after it (while waiting) survive and run once
  @Test def t13197OtherCallbacksSurvive(): Unit = {
    val p = Promise[Int]()
    val runs = new java.util.concurrent.atomic.AtomicInteger
    def cb(): Unit = p.future.onComplete(_ => runs.incrementAndGet())(ExecutionContext.parasitic)
    cb(); cb()
    interruptedAwait(p.future, registeredOn = p, baseline = 2, Inf) { waiter =>
      cb(); cb(); cb() // latch is now in the middle of the callback list
      assertEquals(6, numTransforms(p))
      waiter.interrupt()
    }
    assertEquals(5, numTransforms(p))
    p.success(1)
    assertEquals(5, runs.get)
  }

  // Many threads polling with short timeouts, racing with other threads adding callbacks
  @Test def t13197ConcurrentPollers(): Unit = {
    val p = Promise[Int]()
    val runs = new java.util.concurrent.atomic.AtomicInteger
    val pollers = 8
    val adders = 2
    val callbacksPerAdder = 500
    val start = new java.util.concurrent.CountDownLatch(1)
    val joins =
      List.fill(pollers)(fork {
        start.await()
        for (i <- 1 to 1000)
          assertThrows[TimeoutException](Await.ready(p.future, if (i % 10 == 0) 1.millisecond else 1.nanosecond))
      }) ++ List.fill(adders)(fork {
        start.await()
        for (_ <- 1 to callbacksPerAdder) p.future.onComplete(_ => runs.incrementAndGet())(ExecutionContext.parasitic)
      })
    start.countDown()
    joins.foreach(_())
    assertEquals(adders * callbacksPerAdder, numTransforms(p))
    p.success(1)
    assertEquals(adders * callbacksPerAdder, runs.get)
  }

  // Concurrent pollers on an already-linked future clean up the root
  @Test def t13197ConcurrentPollersLinked(): Unit = {
    val inner = Promise[Int]()
    val (gate, outer) = linked(inner.future)
    gate.success(())
    outer.onComplete(_ => ())(ExecutionContext.parasitic)
    val joins = List.fill(8)(fork {
      for (_ <- 1 to 1000) assertThrows[TimeoutException](Await.ready(inner.future, 1.nanosecond))
    })
    joins.foreach(_())
    assertEquals(1, numTransforms(outer))
  }

  // Pollers racing with completion: each call either times out or sees the value, nothing else
  @Test def t13197PollersRacingCompletion(): Unit = for (_ <- 1 to 20) {
    val p = Promise[Int]()
    val start = new java.util.concurrent.CountDownLatch(1)
    val joins = List.fill(4)(fork {
      start.await()
      var done = false
      while (!done)
        try { assertEquals(42, Await.result(p.future, 1.microsecond)); done = true }
        catch { case _: TimeoutException => }
    })
    start.countDown()
    Thread.`yield`()
    p.success(42)
    joins.foreach(_())
  }

  // Unregistering races with linking, which moves the callback from `inner` to `outer` in two steps:
  // `linkRootOf` publishes the `Link` before adding the callbacks to the root. An unregister in between
  // follows the link and misses the callback at the root; `linkRootOf` must then remove it.
  @Test def t13197UnregisterRacingLink(): Unit = {
    val iterations = 200000
    val gates = new Array[Promise[Unit]](iterations)
    val outers = new Array[Future[Int]](iterations)
    val deregs = new Array[() => Unit](iterations)
    for (i <- 0 until iterations) {
      val inner = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      val (gate, outer) = linked(inner)
      gates(i) = gate; outers(i) = outer
      deregs(i) = inner.onCompleteWithUnregister(_ => ())(ExecutionContext.parasitic)
    }
    // two threads step through the iterations in lockstep, one linking and one unregistering
    val ticket = new java.util.concurrent.atomic.AtomicInteger
    def lockstep(i: Int): Unit = { ticket.incrementAndGet(); while (ticket.get < 2 * (i + 1)) () }
    val join = fork(for (i <- 0 until iterations) { lockstep(i); gates(i).success(()) })
    for (i <- 0 until iterations) { lockstep(i); deregs(i)() }
    join()
    val leaks = outers.count(numTransforms(_) != 0)
    assertEquals("leaked callbacks", 0, leaks)
  }

  // Unregistering is idempotent, and a no-op once completed (also via a completed link root)
  @Test def t13197UnregisterIdempotent(): Unit = {
    locally {
      val p = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      val dereg = p.onCompleteWithUnregister(_ => ())(ExecutionContext.parasitic)
      p.onComplete(_ => ())(ExecutionContext.parasitic)
      dereg(); dereg()
      assertEquals(1, numTransforms(p))
      p.success(1)
      dereg()
      assertEquals(Some(Success(1)), p.value)
    }
    locally {
      val inner = Promise[Int]().asInstanceOf[DefaultPromise[Int]]
      val (gate, outer) = linked(inner)
      val dereg = inner.onCompleteWithUnregister(_ => ())(ExecutionContext.parasitic)
      gate.success(())
      assertTrue(isLinked(inner))
      inner.success(1) // completes the root
      dereg() // finds a completed root, unlinks `inner`
      assertEquals(Some(Success(1)), inner.value)
      assertEquals(Some(Success(1)), outer.value)
    }
  }

  @Test def completedWaitUndefined(): Unit = {
    assertThrows[IllegalArgumentException](Await.result(Future.successful(1), Undefined))
    assertThrows[IllegalArgumentException](Await.ready(Future.successful(1), Undefined))
  }
}
