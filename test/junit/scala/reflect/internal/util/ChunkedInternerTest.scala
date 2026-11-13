package scala.reflect.internal.util

import org.junit.Assert._
import org.junit.Assume.assumeTrue
import org.junit.Test

class ChunkedInternerTest {
  /** Equal by `n` alone, with a hash chosen by the test. */
  final class Key(val n: Int, val h: Int) {
    override def equals(other: Any): Boolean = other match {
      case k: Key => k.n == n
      case _      => false
    }
    override def hashCode: Int = h
  }

  private def newInterner = new ChunkedInterner[Key](16, weak = true)

  private def intern(t: ChunkedInterner[Key], n: Int, h: Int): Key = t.findOrAdd(new Key(n, h))

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
    val n = ChunkedInterner.ChunkSize * 2 // needs four chunks at a load of at most 1/2
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

  @Test def strongKeepsElements(): Unit = {
    val t = new ChunkedInterner[Key](16, weak = false)
    for (i <- 0 until 200000) intern(t, i, i) // garbage once interned, but held strongly
    System.gc()
    assertEquals(200000, t.size)
    val a = intern(t, -1, 7)
    assertSame(a, intern(t, -1, 7))
    assertEquals(200001, t.size)
  }

  /** Intern (n, h), running `between` after the failed `find` and before the `add`, as a constructor that interns
   *  its parts would. */
  private def internAround(t: ChunkedInterner[Key], n: Int, h: Int)(between: => Unit): Key = {
    val k = new Key(n, h)
    val e = t.find(h, k)
    if (e != null) e
    else {
      between
      t.add(k, h)
    }
  }

  @Test def nestedInternBetweenFindAndAdd(): Unit = {
    for (weak <- List(true, false)) {
      val t = new ChunkedInterner[Key](16, weak)
      var nested: List[Key] = Nil
      // same hash: the nested add takes the slot the outer find saw as free
      val a = internAround(t, 1, 5) { nested ::= intern(t, 2, 5) }
      // another hash: a nested failed find moves the free slot elsewhere
      val b = internAround(t, 3, 6) { nested ::= internAround(t, 4, 9)(()) }
      // and a nested find that fails without an add
      val c = internAround(t, 5, 7)(t.find(7, new Key(99, 7)))
      val List(d, e) = nested
      for ((n, h, k) <- List((1, 5, a), (3, 6, b), (5, 7, c), (4, 9, d), (2, 5, e))) assertSame(k, intern(t, n, h))
      assertEquals(5, t.size)
    }
  }

  @Test def clearForgetsEverything(): Unit = {
    val t = newInterner
    val a = intern(t, 1, 1)
    t.clear()
    assertEquals(0, t.size)
    assertNotSame(a, intern(t, 1, 1))
  }
}
