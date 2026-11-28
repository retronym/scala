package scala.concurrent

import org.junit.Assert.assertTrue
import org.junit.Test

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.DurationInt

class BatchingExecutorBlockOnTest {
  private def batchable(body: => Unit): Runnable = new Runnable with Batchable { def run(): Unit = body }

  /** Tasks submitted to the batching executor from inside a `blocking` section must not be queued up
   *  behind the blocking section, since the blocked code may be waiting for exactly those tasks.
   */
  @Test def tasksSubmittedWhileBlockedAreNotQueuedBehindTheBlock(): Unit = {
    val ec = ExecutionContext.opportunistic
    val innerRan   = new CountDownLatch(1)
    val outerDone  = Promise[Boolean]()
    ec.execute(batchable {
      outerDone.complete(scala.util.Try(blocking {
        ec.execute(batchable(innerRan.countDown()))
        Future(innerRan.countDown())(ec) // the same, via a Transformation
        innerRan.await(10, SECONDS)      // a wait that is not itself `blocking`-aware
      }))
    })
    assertTrue("inner tasks must run while the outer task is blocked", Await.result(outerDone.future, 30.seconds))
  }

  /** After the blocking section, nested submissions are batched on the current thread again. */
  @Test def batchingResumesAfterBlocking(): Unit = {
    val ec = ExecutionContext.opportunistic
    val p = Promise[(Thread, Thread)]()
    ec.execute(batchable {
      blocking(())
      val outer = Thread.currentThread
      ec.execute(batchable(p.success((outer, Thread.currentThread))))
    })
    val (outer, inner) = Await.result(p.future, 30.seconds)
    assertTrue("nested Batchable should run on the submitting thread once the batch resumes", outer eq inner)
  }
}
