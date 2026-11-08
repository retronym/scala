package scala.reflect.runtime

import java.lang.ref.WeakReference

import org.junit.Assert._
import org.junit.Test

class ScratchTest {
  @Test def scratchIsPerThread(): Unit = {
    val u = new JavaUniverse
    val here = u.scratch
    assertSame(here, u.scratch)
    var there: AnyRef = null
    val t = new Thread(() => there = u.scratch)
    t.start(); t.join()
    assertNotNull(there)
    assertNotSame(here, there)
  }

  // Concurrent lubs shared the collector in `GlbLubs.maxTypes`; one thread's `collect` restored
  // `result` under another's traversal, which then failed with a NullPointerException.
  @Test def concurrentLubs(): Unit = {
    val u = new JavaUniverse
    import u._
    val m = runtimeMirror(getClass.getClassLoader)
    def tpe(name: String) = m.staticClass(name).toType
    val ts = List("scala.collection.immutable.List", "scala.collection.immutable.Vector", "scala.collection.mutable.ArrayBuffer", "scala.collection.immutable.Queue")
      .map(n => appliedType(tpe(n), definitions.IntTpe))
    val expected = lub(ts)
    val failures = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]
    val threads = List.fill(4)(new Thread(() =>
      try for (_ <- 1 to 30) { val l = lub(ts); if (!(l =:= expected)) throw new AssertionError(s"$l != $expected") }
      catch { case t: Throwable => failures.add(t) }))
    threads.foreach(_.start()); threads.foreach(_.join())
    if (!failures.isEmpty) throw failures.peek()
  }

  // A thread that outlives a universe, e.g. one in a pool, must not keep the universe alive
  // through the scratch state it used.
  @Test def scratchDoesNotRetainUniverse(): Unit = {
    def useAndDrop(): WeakReference[AnyRef] = {
      val u = new JavaUniverse
      import u._
      assertTrue(definitions.IntTpe <:< definitions.AnyTpe)
      new WeakReference(u)
    }
    val ref = useAndDrop()
    var i = 0
    while (ref.get != null && i < 50) {
      System.gc()
      Thread.sleep(20)
      i += 1
    }
    assertNull("universe is retained by this thread", ref.get)
  }
}
