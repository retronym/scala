package scala.tools.nsc.ast.parser

import org.junit.Assert._
import org.junit.Test

class CbufTest {
  private val C = Cbuf.TargetCapacity
  private val samples = List(
    "", "a", "foo_=", "$colon$colon", "名前", "x😀y",
    "x" * (C - 1), "x" * C, "x" * (C + 1), "名" * (C * 5))

  private def filled(s: String): Cbuf = {
    val b = Cbuf.create()
    s.foreach(c => b.append(c))
    b
  }

  private def assertHolds(s: String, b: Cbuf): Unit = {
    assertEquals(s.take(20), s.hashCode, b.hash)
    assertEquals(s, b.toString)
    assertEquals(s.take(20), s.length, b.length)
    assertEquals(s.take(20), s.isEmpty, b.isEmpty)
    if (!s.isEmpty) assertEquals(s.take(20), s.last, b.last)
    assertEquals(s, new String(b.array, 0, b.length))
  }

  // The hash goes straight to `newTermNameHashed`, which would otherwise look in the wrong bucket and enter a duplicate.
  @Test def agreesWithString(): Unit = samples.foreach(s => assertHolds(s, filled(s)))

  @Test def clearForgetsContentsAndHash(): Unit =
    for (s <- samples; next <- samples) {
      val b = filled(s)
      b.clear()
      assertHolds("", b)
      next.foreach(c => b.append(c))
      assertHolds(next, b)
    }
}
