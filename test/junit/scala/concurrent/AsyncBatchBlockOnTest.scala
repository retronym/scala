/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent

import java.util.ArrayDeque
import java.util.concurrent.{CountDownLatch, TimeUnit}

import org.junit.Assert._
import org.junit.Test

class AsyncBatchBlockOnTest {
  // An asynchronous batching executor whose underlying executor is a queue that the test drains by hand
  private final class HandDriven extends ExecutionContext with BatchingExecutor {
    val submitted = new ArrayDeque[Runnable]()
    override protected def submitForExecution(runnable: Runnable): Unit = submitted.add(runnable)
    override def execute(runnable: Runnable): Unit = submitAsyncBatched(runnable)
    override def reportFailure(cause: Throwable): Unit = throw cause
    def runNext(): Unit = submitted.poll().run()
  }

  private def task(body: => Unit): Runnable = new Runnable { def run(): Unit = body }

  // A task submitted from within `blocking` must go to the underlying executor. If it were added to the batch of
  // the blocked thread it could not run before the block returns, and a block that waits for it would never return.
  @Test def submitInsideBlockingIsNotBatched(): Unit = {
    val ec = new HandDriven
    var insideBlocking, afterNestedBlocking, afterBlocking = -1
    var batchedTaskRan = false
    ec.execute(task {
      blocking {
        ec.execute(task(()))
        insideBlocking = ec.submitted.size
        blocking(())
        ec.execute(task(()))
        afterNestedBlocking = ec.submitted.size
      }
      ec.execute(task { batchedTaskRan = true })
      afterBlocking = ec.submitted.size
    })
    ec.runNext()
    assertEquals(1, insideBlocking)
    assertEquals(2, afterNestedBlocking)
    assertEquals(2, afterBlocking) // batched again once the block has returned
    assertTrue(batchedTaskRan)
    assertEquals(2, ec.submitted.size)
  }

  @Test def blockingCanWaitForTaskSubmittedInside(): Unit = {
    val ec = ExecutionContext.opportunistic
    val done = Promise[Boolean]()
    Future.unit.map { _ =>
      val l = new CountDownLatch(1)
      done.success(blocking {
        Future.unit.map(_ => l.countDown())(ec)
        l.await(10, TimeUnit.SECONDS)
      })
    }(ec)
    assertTrue(Await.result(done.future, duration.Duration(30, TimeUnit.SECONDS)))
  }
}
