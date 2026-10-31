package scala.concurrent

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CancellationException, CompletableFuture, CompletionException, TimeUnit}

import org.junit.Assert._
import org.junit.Test

import scala.jdk.FutureConverters._
import scala.jdk.javaapi.{FutureConverters => J}
import scala.tools.testkit.AssertUtil.assertThrows
import scala.util.{Failure, Success}

class FutureConvertersTest {

  private def joinCause(cf: CompletableFuture[_]): Throwable =
    try { cf.join(); throw new AssertionError("expected exceptional completion") }
    catch { case ce: CompletionException => ce.getCause }

  // ---- Scala to Java ----

  @Test def dependentStagesDoNotRunOnCompletingThread(): Unit = {
    val p = Promise[Int]()
    val cf = p.future.asJava.toCompletableFuture
    val ran = new AtomicReference[Thread]
    val accepted = new AtomicReference[Thread]
    val applied = cf.thenApply[Int] { x => ran.set(Thread.currentThread); x + 1 }
    val consumed = cf.thenAccept { _ => accepted.set(Thread.currentThread) }
    p.success(1)
    assertEquals(2, applied.get(30, TimeUnit.SECONDS))
    consumed.get(30, TimeUnit.SECONDS)
    assertNotSame(Thread.currentThread, ran.get)
    assertNotSame(Thread.currentThread, accepted.get)
  }

  @Test def dependentStagesOfCompletedFutureDoNotRunOnCallingThread(): Unit = {
    val cf = Future.successful(1).asJava.toCompletableFuture
    val ran = new AtomicReference[Thread]
    val applied = cf.thenApply[Int] { x => ran.set(Thread.currentThread); x + 1 }
    assertEquals(2, applied.get(30, TimeUnit.SECONDS))
    assertNotSame(Thread.currentThread, ran.get)
  }

  @Test def obtrudeIsUnsupported(): Unit = {
    val cf = Promise[Int]().future.asJava.toCompletableFuture
    assertThrows[UnsupportedOperationException](cf.obtrudeValue(1))
    assertThrows[UnsupportedOperationException](cf.obtrudeException(new RuntimeException))
  }

  @Test def getUsesBlocking(): Unit = {
    val cf = Future.successful(1).asJava.toCompletableFuture
    val count = new AtomicInteger
    val bc = new BlockContext {
      override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = { count.incrementAndGet(); thunk }
    }
    BlockContext.withBlockContext(bc) {
      assertEquals(1, cf.get())
      assertEquals(1, cf.get(1, TimeUnit.SECONDS))
    }
    assertEquals(2, count.get)
  }

  @Test def toCompletableFutureIsIdentity(): Unit = {
    val cs = Promise[Int]().future.asJava
    assertSame(cs, cs.toCompletableFuture)
  }

  @Test def completedFutureConvertsToDoneCompletableFuture(): Unit = {
    val cf = J.asJava(Future.successful("a")).toCompletableFuture
    assertTrue(cf.isDone)
    assertEquals("a", cf.getNow("b"))
  }

  @Test def pendingFutureCompletesCompletableFuture(): Unit = {
    val p = Promise[String]()
    val cf = J.asJava(p.future).toCompletableFuture
    assertFalse(cf.isDone)
    p.success("a")
    assertTrue("completed synchronously by the completing thread", cf.isDone)
    assertEquals("a", cf.join())
  }

  @Test def failedFutureConvertsToExceptionalCompletableFuture(): Unit = {
    val e = new IllegalStateException
    val cf = Future.failed[Int](e).asJava.toCompletableFuture
    assertTrue(cf.isCompletedExceptionally)
    assertSame(e, joinCause(cf))
    try { cf.get(); fail() } catch { case ee: ExecutionException => assertSame(e, ee.getCause) }
  }

  @Test def interruptedFailureIsBoxedInCompletableFuture(): Unit = {
    val ie = new InterruptedException
    val cf = Future.failed[Int](ie).asJava.toCompletableFuture
    joinCause(cf) match {
      case ee: ExecutionException => assertSame(ie, ee.getCause)
      case other => fail(s"$other")
    }
  }

  // ---- Java to Scala ----

  @Test def completedCompletableFutureConvertsToCompletedFuture(): Unit = {
    val f = CompletableFuture.completedFuture("a").asScala
    assertEquals(Some(Success("a")), f.value)
    // scala/bug#12918
    assertEquals(Some(Success("b")), CompletableFuture.completedFuture("b").minimalCompletionStage().asScala.value)
  }

  @Test def exceptionalCompletableFutureConvertsToFailure(): Unit = {
    val e = new IllegalStateException
    val cf = new CompletableFuture[Int]
    cf.completeExceptionally(e)
    assertEquals(Some(Failure(e)), cf.asScala.value)
  }

  @Test def pendingCompletableFutureCompletesFuture(): Unit = {
    val cf = new CompletableFuture[Int]
    val f = J.asScala(cf)
    assertFalse(f.isCompleted)
    cf.complete(3)
    assertEquals(Some(Success(3)), f.value)

    val cf2 = new CompletableFuture[Int]
    val f2 = J.asScala(cf2)
    val e = new IllegalStateException
    cf2.completeExceptionally(e)
    assertEquals(Some(Failure(e)), f2.value)
  }

  @Test def dependentStageFailureIsWrappedInCompletionException(): Unit = {
    // Dependent stages store their failure wrapped in a CompletionException, which is not unwrapped
    val e = new IllegalStateException
    val src = new CompletableFuture[Int]
    val dep = src.thenApply[Int](_ + 1)
    src.completeExceptionally(e)
    J.asScala(dep).value match {
      case Some(Failure(ce: CompletionException)) => assertSame(e, ce.getCause)
      case other => fail(s"$other")
    }
  }

  @Test def cancelledCompletableFutureConvertsToFailure(): Unit = {
    val cf = new CompletableFuture[Int]
    val f = cf.asScala
    cf.cancel(true)
    f.value match {
      case Some(Failure(_: CancellationException)) =>
      case other => fail(s"$other")
    }
  }

  @Test def interruptedCompletableFutureIsBoxed(): Unit = {
    val ie = new InterruptedException
    val cf = new CompletableFuture[Int]
    cf.completeExceptionally(ie)
    cf.asScala.value match {
      case Some(Failure(ee: ExecutionException)) => assertSame(ie, ee.getCause)
      case other => fail(s"$other")
    }
  }

  @Test def roundTripsAreIdentities(): Unit = {
    for (cf <- List(new CompletableFuture[Int], CompletableFuture.completedFuture(1))) {
      assertSame(cf, J.asJava(J.asScala(cf)))
      assertSame(cf, cf.asScala.asJava)
    }
    for (f <- List(Promise[Int]().future, Future.successful(1))) {
      assertSame(f, J.asScala(J.asJava(f)))
      assertSame(f, f.asJava.asScala)
    }
  }
}
