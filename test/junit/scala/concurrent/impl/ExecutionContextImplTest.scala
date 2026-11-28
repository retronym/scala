package scala.concurrent.impl

import java.lang.ref.WeakReference
import java.net.{URL, URLClassLoader}

import org.junit.Assert.assertTrue
import org.junit.Test

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}

class ExecutionContextImplTest {
  import ExecutionContextImplTest._

  /** The default pool (the pool behind `ExecutionContext.global`) outlives the code that submits work to it. A worker
   *  thread must not keep the class loader of the thread whose submission caused the worker to be created: today it
   *  inherits that thread's context class loader, and keeps it for as long as the worker lives.
   */
  @Test def workerDoesNotRetainContextClassLoaderOfSubmitter(): Unit = {
    val ec = ExecutionContext.fromExecutorService(null)
    try {
      val loader = growPoolFromThreadWithContextLoader(ec)
      assertTrue("a worker thread of the pool keeps the submitter's context class loader reachable", collected(loader))
    } finally ec.shutdown()
  }
}

object ExecutionContextImplTest {
  def growPoolFromThreadWithContextLoader(ec: ExecutionContextExecutorService): WeakReference[ClassLoader] = {
    val loader = new URLClassLoader(Array.empty[URL], getClass.getClassLoader)
    val submitter = new Thread(() => Await.result(Future(())(ec), 10.seconds))
    submitter.setContextClassLoader(loader)
    submitter.start()
    submitter.join(10000L)
    new WeakReference(loader)
  }

  def collected(ref: WeakReference[_]): Boolean = {
    var i = 0
    while (i < 50 && ref.get != null) { System.gc(); Thread.sleep(10); i += 1 }
    ref.get == null
  }
}
