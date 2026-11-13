package scala.reflect.internal.util

import org.junit.Assert._
import org.junit.Assume.assumeTrue
import org.junit.Test

class WeakInternerTest {
  /** Equal by `n` alone, with a hash chosen by the test. */
  final class Key(val n: Int, val h: Int) {
    override def equals(other: Any): Boolean = other match {
      case k: Key => k.n == n
      case _      => false
    }
    override def hashCode: Int = h
  }

  private def newInterner = new WeakInterner[Key](16)

  private def intern(t: WeakInterner[Key], n: Int, h: Int): Key = t.findOrAdd(new Key(n, h))

  @Test def internsByEquality(): Unit = {
    val t = newInterner
    val a = intern(t, 1, 7)
    val b = intern(t, 2, 7) // same hash, different element
    assertNotSame(a, b)
    assertSame(a, intern(t, 1, 7))
    assertSame(b, intern(t, 2, 7))
    assertSame(a, t.find(7, new Key(1, 7)))
    assertNull(t.find(7, new Key(3, 7)))
    assertEquals(2, t.size)
  }

  @Test def growsAcrossChunks(): Unit = {
    val t = newInterner
    val n = WeakInterner.ChunkSize * 2 // needs four chunks at a load of at most 1/2
    // a poor hash, so that probe sequences run across chunk boundaries
    def hash(i: Int) = i * 31 >>> 3
    val keys = Array.tabulate(n)(i => intern(t, i, hash(i)))
    assertTrue(t.chunkCount >= 4)
    for (i <- 0 until n) assertSame(keys(i), intern(t, i, hash(i)))
    assertEquals(n, t.size)
  }

  @Test def dropsCollectedElements(): Unit = {
    val t = newInterner
    val kept = Array.tabulate(1000)(i => intern(t, i, i))
    for (i <- 1000 until 200000) intern(t, i, i) // garbage once interned
    var tries = 0
    while (t.size > 1000 && tries < 50) {
      System.gc()
      intern(t, -1, -1) // drains the reference queue
      tries += 1
    }
    assumeTrue("no weak references were cleared", t.size < 200000)
    for (i <- 0 until 1000) assertSame(kept(i), intern(t, i, i))
  }

  @Test def clearForgetsEverything(): Unit = {
    val t = newInterner
    val a = intern(t, 1, 1)
    t.clear()
    assertEquals(0, t.size)
    assertNotSame(a, intern(t, 1, 1))
  }
}
