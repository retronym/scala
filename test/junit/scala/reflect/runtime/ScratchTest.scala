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
