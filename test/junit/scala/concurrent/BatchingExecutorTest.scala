package scala.concurrent

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, Executor, ForkJoinPool, RejectedExecutionException, TimeUnit}

import org.junit.Assert._
import org.junit.{Assume, Test}

import scala.concurrent.duration._
import scala.tools.testkit.AssertUtil.assertThrows
import scala.util.{Failure, Success, Try}

class BatchingExecutorTest {
  import BatchingExecutorTest._
  import BatchingExecutorStatics.{runLimit, syncPreBatchDepth}

  // ---- parasitic ----

  @Test def parasiticDeepResubmissionDoesNotOverflowStack(): Unit = {
    val n = 100000
    var count = 0
    lazy val r: Runnable = batchable { count += 1; if (count < n) ExecutionContext.parasitic.execute(r) }
    ExecutionContext.parasitic.execute(r)
    assertEquals(n, count)
  }

  @Test def parasiticDeepFanOutRunsAllTasks(): Unit = {
    var count = 0
    def spawn(depth: Int): Runnable = () => {
      count += 1
      if (depth > 0) { ExecutionContext.parasitic.execute(spawn(depth - 1)); ExecutionContext.parasitic.execute(spawn(depth - 1)) }
    }
    ExecutionContext.parasitic.execute(spawn(15))
    assertEquals((1 << 16) - 1, count)
  }

  @Test def parasiticExecuteNullThrowsNPE(): Unit =
    assertThrows[NullPointerException](ExecutionContext.parasitic.execute(null))

  @Test def parasiticRunsSynchronouslyBelowPreBatchDepth(): Unit = {
    var ran = false
    ExecutionContext.parasitic.execute(() => ran = true)
    assertTrue("task should run before execute returns", ran)
  }

  @Test def syncBatchDefersTasksBeyondPreBatchDepth(): Unit = {
    val ex = new SyncExecutor
    val order = new ConcurrentLinkedQueue[String]
    def nest(n: Int): Unit =
      if (n == 0) {
        var ran = false
        ex.execute(() => ran = true)
        // once inside a batch, the task is queued until the current task returns
        order.add(s"ran-synchronously=$ran")
      } else ex.execute(() => nest(n - 1))
    nest(syncPreBatchDepth + 1)
    assertEquals(List("ran-synchronously=false"), List.from(order.toArray))
  }

  @Test def syncBatchRunsAllTasksInSomeOrder(): Unit = {
    val ex = new SyncExecutor
    val order = new ConcurrentLinkedQueue[Int]
    def nest(n: Int): Unit =
      if (n == 0) (1 to 5).foreach(i => ex.execute(() => order.add(i)))
      else ex.execute(() => nest(n - 1))
    nest(syncPreBatchDepth + 1)
    // The order is not specified (currently LIFO within a batch), only that all tasks run
    assertEquals(List(1, 2, 3, 4, 5), List.from(order.toArray(Array.empty[Integer])).map(_.intValue).sorted)
  }

  @Test def syncFailuresAreReportedAndDoNotPreventLaterTasks(): Unit =
    for (depth <- List(0, syncPreBatchDepth + 1)) {
      val ex = new SyncExecutor
      val e1, e2 = new RuntimeException
      val ran = new AtomicInteger
      def nest(n: Int): Unit =
        if (n == 0) {
          ex.execute(() => throw e1)
          ex.execute(() => ran.incrementAndGet())
          ex.execute(() => throw e2)
          ex.execute(() => ran.incrementAndGet())
        } else ex.execute(() => nest(n - 1))
      nest(depth)
      assertEquals(s"depth $depth", 2, ran.get)
      assertEquals(s"depth $depth", Set(e1, e2), ex.failures.toSet)
    }

  // ---- opportunistic ----

  private def opportunistic: ExecutionContext = ExecutionContext.opportunistic

  /** Runs `body` as a batched task on `opportunistic` and returns its outcome. */
  private def inBatch[A](body: => A): A = {
    val result = new AtomicReference[Try[A]]
    val done = new CountDownLatch(1)
    opportunistic.execute(batchable { result.set(Try(body)); done.countDown() })
    assertTrue("batched task did not complete", done.await(30, TimeUnit.SECONDS))
    result.get.get
  }

  @Test def opportunisticBatchesBatchableTasksOnSameThread(): Unit = {
    val innerThread = new AtomicReference[Thread]
    val innerSawOuterDone = new AtomicBoolean
    val outerDone = new AtomicBoolean
    val innerRan = new CountDownLatch(1)
    val outerThread = inBatch {
      opportunistic.execute(batchable {
        innerThread.set(Thread.currentThread)
        innerSawOuterDone.set(outerDone.get)
        innerRan.countDown()
      })
      outerDone.set(true)
      Thread.currentThread
    }
    assertTrue(innerRan.await(30, TimeUnit.SECONDS))
    assertSame(outerThread, innerThread.get)
    assertTrue("batched task should run after the submitting task returns", innerSawOuterDone.get)
  }

  @Test def opportunisticBatchesFutureMapOnSameThread(): Unit = {
    val innerThread = new AtomicReference[Thread]
    val ran = new CountDownLatch(1)
    val outerThread = inBatch {
      Future.unit.map { _ => innerThread.set(Thread.currentThread); ran.countDown() }(opportunistic)
      Thread.currentThread
    }
    assertTrue(ran.await(30, TimeUnit.SECONDS))
    assertSame(outerThread, innerThread.get)
  }

  private def assumeGlobalParallelism(): Unit =
    Assume.assumeTrue("needs a second global worker", ExecutionContext.global.asInstanceOf[ForkJoinPool].getParallelism >= 2)

  @Test def opportunisticDoesNotBatchNonBatchableRunnable(): Unit = {
    assumeGlobalParallelism()
    // If the runnable were batched, it could only run after the outer task returns, so waiting for it would time out
    val ran = inBatch {
      val latch = new CountDownLatch(1)
      opportunistic.execute(() => latch.countDown())
      latch.await(30, TimeUnit.SECONDS)
    }
    assertTrue("non-Batchable runnable should not be queued in the current batch", ran)
  }

  @Test def opportunisticDoesNotBatchOnCompleteCallbacks(): Unit = {
    assumeGlobalParallelism()
    val ran = inBatch {
      val latch = new CountDownLatch(1)
      Future.unit.onComplete(_ => latch.countDown())(opportunistic)
      latch.await(30, TimeUnit.SECONDS)
    }
    assertTrue("onComplete callbacks do not benefit from batching", ran)
  }

  private def queueInCurrentBatch(p: Promise[Int]): Unit =
    opportunistic.execute(batchable { p.success(42) })

  @Test def opportunisticBlockingAwaitOnTaskInSameBatchDoesNotDeadlock(): Unit =
    assertEquals(42, inBatch {
      val p = Promise[Int]()
      queueInCurrentBatch(p)
      blocking { Await.result(p.future, 10.seconds) }
    })

  @Test def opportunisticAwaitOnTaskInSameBatchDoesNotDeadlock(): Unit =
    // Await.result calls `blocking` itself
    assertEquals(42, inBatch {
      val p = Promise[Int]()
      queueInCurrentBatch(p)
      Await.result(p.future, 10.seconds)
    })

  @Test def opportunisticBlockingLatchOnTaskInSameBatchDoesNotDeadlock(): Unit =
    assertTrue(inBatch {
      val latch = new CountDownLatch(1)
      opportunistic.execute(batchable { latch.countDown() })
      blocking { latch.await(10, TimeUnit.SECONDS) }
    })

  @Test def opportunisticUnmanagedWaitOnTaskInSameBatchCannotProgress(): Unit = {
    // Without `blocking`, the queued task cannot run until the current one returns
    val (ranWhileWaiting, ranAfterwards) = {
      val latch = new CountDownLatch(1)
      val r = inBatch {
        opportunistic.execute(batchable { latch.countDown() })
        latch.await(50, TimeUnit.MILLISECONDS)
      }
      (r, latch.await(30, TimeUnit.SECONDS))
    }
    assertFalse(ranWhileWaiting)
    assertTrue(ranAfterwards)
  }

  // ---- custom asynchronous BatchingExecutor ----

  @Test def asyncBatchIsChunkedByRunLimit(): Unit = {
    val q = new QueueExecutor
    val ex = new AsyncExecutor(q)
    val n = 2 * runLimit + 10
    val ran = new AtomicInteger
    ex.execute(() => (1 to n).foreach(_ => ex.execute(() => ran.incrementAndGet())))
    assertEquals(1, ex.submissions.get)
    assertEquals(Success(()), q.runNext())
    assertEquals("first chunk (incl. the submitting task)", runLimit - 1, ran.get)
    assertEquals(2, ex.submissions.get)
    assertEquals(Success(()), q.runNext())
    assertEquals(2 * runLimit - 1, ran.get)
    assertEquals(Success(()), q.runNext())
    assertEquals(n, ran.get)
    assertEquals(3, ex.submissions.get)
    assertTrue(q.isEmpty)
    assertTrue(ex.failures.isEmpty)
  }

  @Test def asyncBatchFailurePropagatesToUnderlyingExecutorAndResubmitsRemainder(): Unit = {
    val q = new QueueExecutor
    val ex = new AsyncExecutor(q)
    val ran = new AtomicInteger
    val boom = new RuntimeException("boom")
    ex.execute(() => { (1 to 5).foreach(_ => ex.execute(() => ran.incrementAndGet())); throw boom })
    q.runNext() match {
      case Failure(t) => assertSame(boom, t)
      case other => fail(s"expected failure to propagate out of the batch, was $other")
    }
    // the failure is not passed to reportFailure, but left to the underlying executor
    assertTrue(ex.failures.isEmpty)
    assertEquals(0, ran.get)
    assertEquals(2, ex.submissions.get)
    assertEquals(Success(()), q.runNext())
    assertEquals(5, ran.get)
    assertTrue(q.isEmpty)
  }

  @Test def asyncBatchFailureWithRejectedResubmission(): Unit = {
    val q = new QueueExecutor
    q.rejectAfter = 1
    val ex = new AsyncExecutor(q)
    val boom = new RuntimeException("boom")
    ex.execute(() => { ex.execute(() => ()); throw boom })
    q.runNext() match {
      case Failure(ee: ExecutionException) =>
        assertSame(boom, ee.getCause)
        assertTrue(ee.getSuppressed.toList.exists(_.isInstanceOf[RejectedExecutionException]))
      case other => fail(s"expected ExecutionException, was $other")
    }
  }

  @Test def asyncBatchBlockingResubmitsPendingTasks(): Unit = {
    val q = new QueueExecutor
    val ex = new AsyncExecutor(q)
    val ran = new AtomicInteger
    val queuedWhileBlocking = new AtomicInteger(-1)
    ex.execute(() => {
      ex.execute(() => ran.incrementAndGet())
      blocking { queuedWhileBlocking.set(q.size) }
      // tasks submitted after blocking go to the (now empty) current batch
      ex.execute(() => ran.incrementAndGet())
    })
    assertEquals(Success(()), q.runNext())
    assertEquals("pending task should be resubmitted before blocking", 1, queuedWhileBlocking.get)
    assertEquals(1, ran.get)
    assertEquals(Success(()), q.runNext())
    assertEquals(2, ran.get)
    assertTrue(q.isEmpty)
  }

  // ---- BlockContext ----

  @Test def withBlockContextIsUsedByBlockingAndRestored(): Unit = {
    val before = BlockContext.current
    val bc = new CountingBlockContext
    assertEquals(3, BlockContext.withBlockContext(bc) {
      assertSame(bc, BlockContext.current)
      blocking(1) + blocking(2)
    })
    assertEquals(2, bc.count.get)
    assertSame(before, BlockContext.current)
    val e = new IllegalStateException
    assertThrows[IllegalStateException](BlockContext.withBlockContext(bc)(throw e), _ eq e.getMessage)
    assertSame(before, BlockContext.current)
  }

  @Test def withBlockContextNests(): Unit = {
    val a, b = new CountingBlockContext
    BlockContext.withBlockContext(a) {
      BlockContext.withBlockContext(b) { assertSame(b, BlockContext.current) }
      assertSame(a, BlockContext.current)
      BlockContext.withBlockContext(a) { assertSame(a, BlockContext.current) }
      assertSame(a, BlockContext.current)
    }
  }

  @Test def blockingPropagatesExceptions(): Unit = {
    val e = new IllegalStateException("thrown")
    val bc = new CountingBlockContext
    BlockContext.withBlockContext(bc) {
      assertThrows[IllegalStateException](blocking(throw e), _ == "thrown")
    }
    assertEquals(1, bc.count.get)
  }

  @Test def currentFallsBackToThreadThenDefault(): Unit = {
    val seen = new AtomicReference[List[BlockContext]]
    val t = new Thread() with BlockContext {
      override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = thunk
      override def run(): Unit = {
        val bc = new CountingBlockContext
        seen.set(List(BlockContext.current, BlockContext.withBlockContext(bc)(BlockContext.current), BlockContext.current))
      }
    }
    t.start()
    t.join(30000)
    seen.get match {
      case List(a, b, c) =>
        assertSame(t, a)
        assertTrue(b.isInstanceOf[CountingBlockContext])
        assertSame(t, c)
      case other => fail(s"$other")
    }

    val plain = new AtomicReference[BlockContext]
    val t2 = new Thread(() => plain.set(BlockContext.current))
    t2.start()
    t2.join(30000)
    assertSame(BlockContext.defaultBlockContext, plain.get)
  }

  @Test def defaultBlockContextJustRunsThunk(): Unit =
    assertEquals(1, BlockContext.withBlockContext(BlockContext.defaultBlockContext)(blocking(1)))

  @Test def usingBlockContextPassesPreviousContext(): Unit = {
    val a, b = new CountingBlockContext
    val outside = BlockContext.current
    BlockContext.usingBlockContext(a) { prev =>
      assertSame(outside, prev)
      assertSame(a, BlockContext.current)
    }
    BlockContext.withBlockContext(a) {
      BlockContext.usingBlockContext(b) { prev =>
        assertSame(a, prev)
        assertSame(b, BlockContext.current)
      }
      assertSame(a, BlockContext.current)
      BlockContext.usingBlockContext(a) { prev => assertSame(a, prev) }
    }
    assertSame(outside, BlockContext.current)
    val e = new IllegalStateException
    assertThrows[IllegalStateException](BlockContext.usingBlockContext(a)(_ => throw e))
    assertSame(outside, BlockContext.current)
  }
}

object BatchingExecutorTest {
  def batchable(body: => Unit): Runnable = new Runnable with Batchable { def run(): Unit = body }

  final class CountingBlockContext extends BlockContext {
    val count = new AtomicInteger
    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = { count.incrementAndGet(); thunk }
  }

  /** Like `parasitic`, but recording failures. */
  final class SyncExecutor extends BatchingExecutor {
    private[this] val _failures = new ConcurrentLinkedQueue[Throwable]
    def failures: List[Throwable] = List.from(_failures.toArray(Array.empty[Throwable]))
    override protected def submitForExecution(runnable: Runnable): Unit = runnable.run()
    override def execute(runnable: Runnable): Unit = submitSyncBatched(runnable)
    override protected def reportFailure(throwable: Throwable): Unit = _failures.add(throwable)
  }

  /** An asynchronous BatchingExecutor (like `opportunistic`, but batching all tasks) over `underlying`. */
  final class AsyncExecutor(underlying: Executor) extends BatchingExecutor {
    val submissions = new AtomicInteger
    private[this] val _failures = new ConcurrentLinkedQueue[Throwable]
    def failures: List[Throwable] = List.from(_failures.toArray(Array.empty[Throwable]))
    override protected def submitForExecution(runnable: Runnable): Unit = { submissions.incrementAndGet(); underlying.execute(runnable) }
    override def execute(runnable: Runnable): Unit = submitAsyncBatched(runnable)
    override protected def reportFailure(throwable: Throwable): Unit = _failures.add(throwable)
  }

  /** An executor whose tasks are run manually, one at a time, by the test thread. */
  final class QueueExecutor extends Executor {
    private[this] val q = new java.util.ArrayDeque[Runnable]
    private[this] var submits = 0
    var rejectAfter = Int.MaxValue
    def execute(r: Runnable): Unit = {
      submits += 1
      if (submits > rejectAfter) throw new RejectedExecutionException("rejected")
      q.add(r)
    }
    def size: Int = q.size
    def isEmpty: Boolean = q.isEmpty
    def runNext(): Try[Unit] = Try(q.poll().run())
  }
}
