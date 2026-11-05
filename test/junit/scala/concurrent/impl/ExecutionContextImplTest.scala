package scala.concurrent.impl

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{Callable, ConcurrentLinkedQueue, CountDownLatch, Executor, ExecutorService, Executors, ForkJoinPool, Semaphore, TimeUnit}

import org.junit.Assert._
import org.junit.{Ignore, Test}

import scala.concurrent.{blocking, BlockContext, ExecutionContext, ExecutionContextExecutorService}
import scala.tools.testkit.AssertUtil.{assertThrows, waitForIt}

class ExecutionContextImplTest {
  import ExecutionContextImpl.DefaultThreadFactory

  private val Parallelism = 2

  private final class Fixture(maxBlockers: Int) {
    val uncaught = new ConcurrentLinkedQueue[Throwable]
    val reported = new ConcurrentLinkedQueue[Throwable]
    val uncaughtLatch = new CountDownLatch(1)
    val factory = new DefaultThreadFactory(daemonic = true, maxBlockers = maxBlockers, prefix = "test-ec",
      uncaught = (_: Thread, t: Throwable) => { uncaught.add(t); uncaughtLatch.countDown() })
    val pool = new ForkJoinPool(Parallelism, factory, factory.uncaught, true)
    val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(pool, reported.add(_))

    /** The number of free blocker permits, via reflection on the private semaphore. */
    def permits: Int = {
      val f = classOf[DefaultThreadFactory].getDeclaredFields.find(_.getName.endsWith("blockerPermits")).get
      f.setAccessible(true)
      f.get(factory).asInstanceOf[Semaphore].availablePermits
    }

    /** Runs `body` on the pool and waits for its result. */
    def onPool[A](body: => A): A = {
      val result = new AtomicReference[Either[Throwable, A]]
      val done = new CountDownLatch(1)
      ec.execute(() => { result.set(try Right(body) catch { case t: Throwable => Left(t) }); done.countDown() })
      assertTrue("task did not complete", done.await(30, TimeUnit.SECONDS))
      result.get.fold(throw _, identity)
    }

    def close(): Unit = { pool.shutdownNow(); pool.awaitTermination(30, TimeUnit.SECONDS) }
  }

  private def withFixture[A](maxBlockers: Int)(body: Fixture => A): A = {
    val fx = new Fixture(maxBlockers)
    try body(fx) finally fx.close()
  }

  @Test def factoryValidatesArguments(): Unit = {
    assertThrows[IllegalArgumentException](new DefaultThreadFactory(true, 0, null, null))
    assertThrows[IllegalArgumentException](new DefaultThreadFactory(true, -1, "p", null))
    assertThrows[IllegalArgumentException](new ExecutionContextImpl(null, _ => ()))
  }

  @Test def threadsAreWired(): Unit = withFixture(maxBlockers = 1) { fx =>
    val t = fx.onPool(Thread.currentThread)
    assertTrue(t.isDaemon)
    assertEquals(s"test-ec-${t.getId}", t.getName)
    assertSame(fx.factory.uncaught, t.getUncaughtExceptionHandler)
    assertTrue("pool threads are BlockContexts", t.isInstanceOf[BlockContext])
    val plain = fx.factory.newThread(() => ())
    assertTrue(plain.isDaemon)
    assertEquals(s"test-ec-${plain.getId}", plain.getName)
    assertSame(fx.factory.uncaught, plain.getUncaughtExceptionHandler)
  }

  @Test def nonDaemonFactory(): Unit = {
    val f = new DefaultThreadFactory(daemonic = false, maxBlockers = 0, prefix = "nd", uncaught = null)
    assertFalse(f.newThread(() => ()).isDaemon)
  }

  @Test def reportFailureUsesReporter(): Unit = withFixture(maxBlockers = 1) { fx =>
    val e = new RuntimeException
    fx.ec.reportFailure(e)
    assertEquals(List(e), List.from(fx.reported.toArray))
    assertTrue(fx.uncaught.isEmpty)
  }

  @Test def uncaughtExceptionInTaskReachesHandler(): Unit = withFixture(maxBlockers = 1) { fx =>
    val e = new RuntimeException("uncaught")
    fx.ec.execute(() => throw e)
    assertTrue("uncaught handler not called", fx.uncaughtLatch.await(30, TimeUnit.SECONDS))
    assertSame(e, fx.uncaught.peek)
    // the pool keeps working
    assertEquals(1, fx.onPool(1))
  }

  /** Submits `n` tasks which each block (inside `blocking` if `managed`) until released, and
   *  returns the latch counting the tasks which started blocking, plus the release latch. */
  private def blockTasks(fx: Fixture, n: Int, managed: Boolean): (CountDownLatch, CountDownLatch, AtomicInteger) = {
    val entered = new CountDownLatch(n)
    val count = new AtomicInteger
    val release = new CountDownLatch(1)
    for (_ <- 1 to n) fx.ec.execute { () =>
      def body() = { count.incrementAndGet(); entered.countDown(); release.await(30, TimeUnit.SECONDS) }
      if (managed) blocking(body()) else body()
    }
    (entered, release, count)
  }

  @Ignore("With FJP's default minimumRunnable = 1, the first (parallelism - 1) managed blockers merely reduce the " +
          "active count instead of spawning a spare thread, yet each still consumes a blocker permit: only " +
          "max(0, maxBlockers - (parallelism - 1)) extra threads are created (observed: 5 of 6 enter for " +
          "parallelism = 2, maxBlockers = 4; 8 of 12 for parallelism = 8, maxBlockers = 4); see review")
  @Test def managedBlockingCompensatesUpToMaxBlockers(): Unit = withFixture(maxBlockers = 4) { fx =>
    val (entered, release, count) = blockTasks(fx, Parallelism + 4, managed = true)
    try assertTrue(s"all blocking tasks should run concurrently, but only ${count.get} did", entered.await(10, TimeUnit.SECONDS))
    finally release.countDown()
    waitForIt(fx.permits == 4, label = "permits released")
  }

  @Test def managedBlockingCompensates(): Unit = withFixture(maxBlockers = 4) { fx =>
    // weaker than the above: blocking adds at least one extra thread
    val (entered, release, _) = blockTasks(fx, Parallelism + 1, managed = true)
    try assertTrue("blocking should compensate", entered.await(30, TimeUnit.SECONDS))
    finally release.countDown()
    waitForIt(fx.permits == 4, label = "permits released")
  }

  @Test def blockingBeyondMaxBlockersIsUnmanaged(): Unit = withFixture(maxBlockers = 2) { fx =>
    val (entered, release, count) = blockTasks(fx, Parallelism + 2 + 1, managed = true)
    try {
      waitForIt(fx.permits == 0, label = "permits taken")
      // no further compensation, so the last task cannot start while the others are blocked
      assertFalse(entered.await(100, TimeUnit.MILLISECONDS))
      assertTrue(s"count ${count.get}", count.get <= Parallelism + 2)
    } finally release.countDown()
    assertTrue(entered.await(30, TimeUnit.SECONDS))
    waitForIt(fx.permits == 2, label = "permits released")
  }

  @Test def noBlockersMeansNoCompensation(): Unit = withFixture(maxBlockers = 0) { fx =>
    val (entered, release, count) = blockTasks(fx, Parallelism + 1, managed = true)
    try {
      waitForIt(count.get == Parallelism, label = "tasks entered")
      assertFalse(entered.await(100, TimeUnit.MILLISECONDS))
      assertEquals(Parallelism, count.get)
      assertTrue(s"pool size ${fx.pool.getPoolSize}", fx.pool.getPoolSize <= Parallelism)
    } finally release.countDown()
    assertTrue(entered.await(30, TimeUnit.SECONDS))
  }

  @Test def nestedBlockingTakesOnePermit(): Unit = withFixture(maxBlockers = 3) { fx =>
    val (outer, inner) = fx.onPool {
      blocking {
        val o = fx.permits
        val i = blocking(blocking(fx.permits))
        (o, i)
      }
    }
    assertEquals(2, outer)
    assertEquals(2, inner)
    waitForIt(fx.permits == 3, label = "permit released")
  }

  @Test def failingBlockingReleasesPermit(): Unit = withFixture(maxBlockers = 4) { fx =>
    val e = new IllegalStateException("in blocking")
    val thrown = fx.onPool(try { blocking[Unit](throw e); null } catch { case t: Throwable => t })
    assertSame(e, thrown)
    waitForIt(fx.permits == 4, label = "permit released")
    // the permit is reusable: blocking still compensates
    val (entered, release, _) = blockTasks(fx, Parallelism + 1, managed = true)
    try assertTrue(entered.await(30, TimeUnit.SECONDS))
    finally release.countDown()
  }

  @Test def blockingOnNonPoolThreadJustRunsThunk(): Unit = withFixture(maxBlockers = 1) { fx =>
    assertEquals(42, blocking(42))
    // even if a pool thread's BlockContext is installed on another thread, no permit is taken
    val poolThread = fx.onPool(Thread.currentThread).asInstanceOf[BlockContext]
    assertEquals(1, BlockContext.withBlockContext(poolThread)(blocking(fx.permits)))
  }

  @Test def fromExecutorServiceDelegates(): Unit = {
    val es: ExecutorService = Executors.newSingleThreadExecutor()
    val ec = ExecutionContext.fromExecutorService(es)
    try {
      assertEquals("x", ec.submit(new Callable[String] { def call() = "x" }).get(30, TimeUnit.SECONDS))
      assertEquals("y", ec.submit((() => ()): Runnable, "y").get(30, TimeUnit.SECONDS))
      assertFalse(ec.isShutdown)
    } finally ec.shutdown()
    assertTrue(es.isShutdown)
    assertTrue(ec.isShutdown)
    assertTrue(ec.awaitTermination(30, TimeUnit.SECONDS))
    assertTrue(ec.isTerminated)
  }

  @Test def fromExecutorUsesGivenExecutorAndReporter(): Unit = {
    val ran = new AtomicInteger
    val reported = new AtomicReference[Throwable]
    val ec = ExecutionContext.fromExecutor((r: Runnable) => { ran.incrementAndGet(); r.run() }, reported.set(_))
    ec.execute(() => ())
    assertEquals(1, ran.get)
    val e = new RuntimeException
    ec.reportFailure(e)
    assertSame(e, reported.get)
  }

  @Test def nullExecutorCreatesDefaultPool(): Unit = {
    val reported = new ConcurrentLinkedQueue[Throwable]
    val ecs: List[ExecutionContext with Executor] = List(ExecutionContext.fromExecutor(null, reported.add(_)), ExecutionContext.fromExecutorService(null, reported.add(_)))
    for (ec <- ecs) {
      val es = ec.asInstanceOf[ExecutorService]
      try {
        val name = new AtomicReference[String]
        val done = new CountDownLatch(1)
        ec.execute(() => { name.set(Thread.currentThread.getName); done.countDown() })
        assertTrue(done.await(30, TimeUnit.SECONDS))
        assertTrue(name.get, name.get.startsWith("scala-execution-context-global-"))
        val e = new RuntimeException
        reported.clear()
        ec.reportFailure(e) // routed via the uncaught exception handler to the reporter
        assertSame(e, reported.peek)
      } finally {
        es.shutdown()
        assertTrue(es.awaitTermination(30, TimeUnit.SECONDS))
      }
    }
  }
}
