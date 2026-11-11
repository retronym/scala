package scala.concurrent

import java.util.concurrent.ConcurrentLinkedQueue

import org.junit.Assert._
import org.junit.{After, Before, Test}

import scala.runtime.NonLocalReturnControl
import scala.tools.testkit.AssertUtil.assertThrows
import scala.util.control.ControlThrowable
import scala.util.{Failure, Success}

/** How `InterruptedException`s and fatal errors thrown by user code (or used to complete promises)
 *  are handled by `Future`/`Promise` and by the synchronous `BatchingExecutor`.
 *
 *  A direct (calling-thread) `ExecutionContext` is used so that the test thread itself runs the
 *  callbacks, which allows observing its interrupt status afterwards.
 */
class InterruptHandlingTest {
  import InterruptHandlingTest._

  private val reported = new ConcurrentLinkedQueue[Throwable]
  private implicit val direct: ExecutionContext = ExecutionContext.fromExecutor(_.run(), reported.add(_))

  @Before def before(): Unit = Thread.interrupted()
  @After def after(): Unit = Thread.interrupted()

  private def reportedList: List[Throwable] = List.from(reported.toArray(Array.empty[Throwable]))

  private def assertBoxedInterrupt(label: String, mode: Mode, f: Future[_]): Unit = f.value match {
    case Some(Failure(ee: ExecutionException)) =>
      assertEquals(label, "Boxed Exception", ee.getMessage)
      assertTrue(s"$label: cause ${ee.getCause}", ee.getCause.isInstanceOf[InterruptedException])
      mode.checkCause(label, ee.getCause)
    case other => fail(s"$label: expected boxed InterruptedException, was $other")
  }

  private def assertInterruptRestored(label: String): Unit =
    assertTrue(s"$label: interrupt status should have been restored", Thread.interrupted())

  private val src = Future.successful(1)
  private val failedSrc = Future.failed[Int](new RuntimeException("source failure"))

  /** Transformations whose result Future is expected to fail with the boxed `InterruptedException`.
   *  The `Boolean` says whether the source future should be failed. */
  private def failingTransformations(raise: () => Nothing): List[(String, Boolean, Future[Int] => Future[Any])] = List(
    ("map", false, _.map(_ => raise())),
    ("flatMap", false, _.flatMap(_ => raise())),
    ("transform", false, _.transform(_ => raise())),
    ("transform(s, f)", false, _.transform(_ => raise(), identity)),
    ("transform(s, f) failure", true, _.transform(identity, _ => raise())),
    ("transformWith", false, _.transformWith(_ => raise())),
    ("filter", false, _.filter(_ => raise())),
    ("collect", false, _.collect { case _ => raise() }),
    ("recover", true, _.recover { case _ => raise() }),
    ("recoverWith", true, _.recoverWith { case _ => raise() }),
    ("zipWith (completed)", false, f => f.zipWith(src)((_, _) => raise())),
    ("Future.apply", false, _ => Future(raise())),
    ("Future.delegate", false, _ => Future.delegate(raise())),
    ("Future.foldLeft", false, f => Future.foldLeft(List(f))(0)((_, _) => raise())),
  )

  @Test def transformationsBoxInterruptedExceptionAndRestoreInterruptStatus(): Unit =
    for (mode <- Modes; (name, useFailed, xform) <- failingTransformations(() => mode.raise())) {
      val label = s"$name/$mode"
      reported.clear()
      val f = xform(if (useFailed) failedSrc else src)
      assertInterruptRestored(label)
      assertBoxedInterrupt(label, mode, f)
      assertTrue(s"$label: nothing should be reported, was $reportedList", reported.isEmpty)
    }

  /** Transformations which do not depend on the source future; zipWith on a pending future is tested separately. */
  private val IgnoresSource = Set("zipWith (completed)", "Future.apply", "Future.delegate")

  @Test def transformationsOnPendingFutureBoxInterruptedExceptionAndRestoreInterruptStatus(): Unit =
    for (mode <- Modes; (name, useFailed, xform) <- failingTransformations(() => mode.raise()) if !IgnoresSource(name)) {
      val label = s"$name/$mode (pending)"
      val p = Promise[Int]()
      val f = xform(p.future)
      assertFalse(label, Thread.interrupted())
      assertFalse(label, f.isCompleted)
      p.complete(if (useFailed) failedSrc.value.get else src.value.get) // the completing thread runs the callback
      assertInterruptRestored(label)
      assertBoxedInterrupt(label, mode, f)
    }

  @Test def zipWithPendingBoxesInterruptedException(): Unit =
    for (mode <- Modes; completeRightFirst <- List(true, false)) {
      val label = s"zipWith/$mode completeRightFirst=$completeRightFirst"
      val p1, p2 = Promise[Int]()
      val f = p1.future.zipWith(p2.future)((_, _) => mode.raise())
      if (completeRightFirst) { p2.success(2); p1.success(1) } else { p1.success(1); p2.success(2) }
      assertInterruptRestored(label)
      assertBoxedInterrupt(label, mode, f)
    }

  @Test def foreachAndOnCompleteReportInterruptedExceptionAndRestoreInterruptStatus(): Unit =
    for (mode <- Modes; pending <- List(false, true)) {
      val label = s"$mode pending=$pending"
      def check(name: String)(register: Future[Int] => Unit): Unit = {
        reported.clear()
        val p = Promise[Int]()
        if (!pending) p.success(1)
        register(p.future)
        if (pending) { assertFalse(Thread.interrupted()); p.success(1) }
        assertInterruptRestored(s"$name/$label")
        reportedList match {
          case List(ie: InterruptedException) => mode.checkCause(s"$name/$label", ie)
          case other => fail(s"$name/$label: expected a reported InterruptedException, was $other")
        }
      }
      check("foreach")(_.foreach(_ => mode.raise()))
      check("onComplete")(_.onComplete(_ => mode.raise()))
    }

  @Test def andThenInterruptedPassesResultThroughReportsAndRestoresInterruptStatus(): Unit =
    for (mode <- Modes; source <- List(src, failedSrc)) {
      reported.clear()
      val f = source.andThen { case _ => mode.raise() }
      assertInterruptRestored(s"andThen/$mode")
      assertEquals(source.value, f.value)
      reportedList match {
        case List(ie: InterruptedException) => mode.checkCause(s"andThen/$mode", ie)
        case other => fail(s"andThen/$mode: expected a reported InterruptedException, was $other")
      }
    }

  @Test def interruptedCallbackDoesNotStopOtherCallbacks(): Unit = {
    val p = Promise[Int]()
    val ran = new ConcurrentLinkedQueue[String]
    p.future.foreach(_ => ran.add("a"))
    p.future.foreach(_ => throw new InterruptedException)
    p.future.foreach(_ => ran.add("b"))
    p.success(1)
    assertInterruptRestored("multiple callbacks")
    assertEquals(Set("a", "b"), Set.from(ran.toArray(Array.empty[String])))
  }

  // ---- Promise completion values ----

  @Test def promiseFailureWithInterruptedExceptionIsBoxed(): Unit = {
    val ie = new InterruptedException
    for (f <- List(Promise[Int]().failure(ie).future, Future.failed[Int](ie), Future.fromTry[Int](Failure(ie)), Promise.failed[Int](ie).future)) {
      f.value match {
        case Some(Failure(ee: ExecutionException)) =>
          assertEquals("Boxed Exception", ee.getMessage)
          assertSame(ie, ee.getCause)
        case other => fail(s"Expected boxed failure, was $other")
      }
      assertFalse("completing with a Failure(IE) must not touch the interrupt status", Thread.interrupted())
    }
  }

  @Test def promiseFailureWithErrorOrControlThrowableIsBoxed(): Unit =
    for (t <- List[Throwable](new Error("boom"), new OutOfMemoryError("fake"), new LinkageError("fake"), new ControlThrowable {})) {
      Promise[Int]().failure(t).future.value match {
        case Some(Failure(ee: ExecutionException)) => assertSame(t, ee.getCause)
        case other => fail(s"Expected boxed failure for $t, was $other")
      }
    }

  @Test def promiseFailureWithNonFatalIsNotBoxed(): Unit = {
    val e = new IllegalStateException
    assertEquals(Some(Failure(e)), Promise[Int]().failure(e).future.value)
  }

  @Test def promiseFailureWithNonLocalReturnControlIsSuccess(): Unit = {
    val key = new AnyRef
    assertEquals(Some(Success(42)), Promise[Int]().failure(new NonLocalReturnControl(key, 42)).future.value)
    assertEquals(Some(Success(42)), Future.failed[Int](new NonLocalReturnControl(key, 42)).value)
  }

  @Test def promiseCompleteWithNullThrowsNPE(): Unit = {
    val p = Promise[Int]()
    assertThrows[NullPointerException](p.complete(null))
    assertThrows[NullPointerException](p.tryComplete(null))
    assertThrows[NullPointerException](Promise.fromTry[Int](null))
    assertFalse(p.isCompleted)
  }

  @Test def promiseSuccessIsPassedThroughUntouched(): Unit = {
    val s = Success("x")
    val p = Promise[String]().complete(s)
    assertSame(s, p.future.value.get)
    assertSame(s, Future.fromTry(s).value.get)
  }

  // ---- Fatal errors ----

  @Test def fatalErrorsFromCallbacksPropagateOutOfExecute(): Unit = {
    def fatals: List[Throwable] = List(new OutOfMemoryError("fake"), new InternalError("fake"), new LinkageError("fake"),
      new ControlThrowable {}, new NonLocalReturnControl(new AnyRef, 1))
    for (t <- fatals) {
      // completed source: dispatch happens within `map`
      assertThrown(t)(src.map(_ => throw t))
      // pending source: dispatch happens within `success`, the transformation stays incomplete
      val p = Promise[Int]()
      val f = p.future.map(_ => throw t)
      assertThrown(t)(p.success(1))
      assertTrue(p.isCompleted)
      assertFalse(s"$t: transformation should remain incomplete", f.isCompleted)
      assertTrue(reported.isEmpty)
    }
  }

  private def assertThrown(expected: Throwable)(body: => Any): Unit =
    try { body; fail(s"Expected $expected to be thrown") }
    catch { case t: Throwable if t eq expected => }

  // ---- BatchingExecutor (synchronous, i.e. `parasitic`-like) ----

  @Test def syncBatchingExecutorRestoresInterruptStatus(): Unit = {
    val ex = new RecordingSyncExecutor
    val ie = new InterruptedException
    ex.execute(() => throw ie)
    assertEquals(List(ie), ex.failures)
    assertInterruptRestored("sync batching executor, unbatched")
  }

  @Test def syncBatchingExecutorRestoresInterruptStatusInBatch(): Unit = {
    val ex = new RecordingSyncExecutor
    val ie = new InterruptedException
    var ranAfter = false
    // Nest deeper than syncPreBatchDepth so that the throwing task runs inside a SyncBatch
    def nest(n: Int): Unit =
      if (n == 0) { ex.execute(() => throw ie); ex.execute(() => ranAfter = true) }
      else ex.execute(() => nest(n - 1))
    nest(BatchingExecutorStatics.syncPreBatchDepth + 2)
    assertEquals(List(ie), ex.failures)
    assertTrue("subsequent tasks should run", ranAfter)
    assertInterruptRestored("sync batching executor, batched")
  }

  @Test def parasiticTransformationRestoresInterruptStatus(): Unit = {
    val f = src.map(_ => Throw.raise())(ExecutionContext.parasitic)
    assertInterruptRestored("parasitic map")
    assertBoxedInterrupt("parasitic map", Throw, f)
  }
}

object InterruptHandlingTest {
  sealed abstract class Mode {
    def raise(): Nothing
    def checkCause(label: String, t: Throwable): Unit
  }
  /** User code throws an InterruptedException itself. */
  object Throw extends Mode {
    final val ie = new InterruptedException("thrown by user code")
    def raise(): Nothing = throw ie
    def checkCause(label: String, t: Throwable): Unit = assertSame(label, ie, t)
    override def toString = "throw"
  }
  /** User code is interrupted while blocked. */
  object Interrupted extends Mode {
    def raise(): Nothing = {
      Thread.currentThread.interrupt()
      Thread.sleep(60000)
      throw new AssertionError("sleep should have been interrupted")
    }
    def checkCause(label: String, t: Throwable): Unit = assertNotSame(label, Throw.ie, t)
    override def toString = "interrupted"
  }
  val Modes: List[Mode] = List(Throw, Interrupted)

  /** A synchronous BatchingExecutor like `parasitic`, but with a recording `reportFailure`. */
  final class RecordingSyncExecutor extends BatchingExecutor {
    private[this] val _failures = new ConcurrentLinkedQueue[Throwable]
    def failures: List[Throwable] = List.from(_failures.toArray(Array.empty[Throwable]))
    override protected def submitForExecution(runnable: Runnable): Unit = runnable.run()
    override def execute(runnable: Runnable): Unit = submitSyncBatched(runnable)
    override protected def reportFailure(throwable: Throwable): Unit = _failures.add(throwable)
  }
}
