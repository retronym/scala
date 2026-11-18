package scala.reflect.internal.util

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CompactRangePositionTest {
  private val source = new BatchSourceFile("", "x")

  private def compact(start: Int, point: Int, end: Int): Boolean =
    Position.range(source, start, point, end).isInstanceOf[CompactRangePosition]

  private def checkRoundTrip(start: Int, point: Int, end: Int): Unit = {
    val pos = Position.range(source, start, point, end)
    val reference = new RangePosition(source, start, point, end)
    assertTrue(pos.isRange && pos.isOpaqueRange && !pos.isTransparent)
    assertEquals(start, pos.start)
    assertEquals(point, pos.point)
    assertEquals(end, pos.end)
    // equal to the unpacked encoding in both directions, with the same hash and rendering
    assertEquals(reference, pos)
    assertEquals(pos, reference)
    assertEquals(reference.hashCode, pos.hashCode)
    assertEquals(reference.toString, pos.toString)
  }

  @Test def packsSmallRanges(): Unit = {
    assertTrue(compact(0, 0, 0))
    assertTrue(compact(10, 10, 11))
    assertTrue(compact(10, 12, 20))
    // largest values of each field: start < 2^18, length < 2^9, point delta < 2^5
    assertTrue(compact((1 << 18) - 1, (1 << 18) - 1, (1 << 18) - 1))
    assertTrue(compact(0, 0, (1 << 9) - 1))
    assertTrue(compact(0, (1 << 5) - 1, (1 << 9) - 1))
    assertTrue(compact((1 << 18) - 1, (1 << 18) - 1 + 31, (1 << 18) - 1 + 511))
  }

  @Test def fallsBackWhenAFieldOverflows(): Unit = {
    assertFalse(compact(1 << 18, 1 << 18, (1 << 18) + 1))  // start
    assertFalse(compact(0, 0, 1 << 9))                     // length
    assertFalse(compact(0, 1 << 5, 100))                   // point delta
    assertFalse(compact(5, 4, 10))                         // point before start
    assertFalse(compact(0, 0, Int.MaxValue))
    assertTrue(Position.range(source, 1 << 18, 1 << 18, 1 << 19).isRange)
  }

  @Test def roundTrips(): Unit = {
    val edges = Seq(0, 1, 2, 31, 32, 255, 511, 512, (1 << 18) - 1, 1 << 18, (1 << 18) + 1, 1 << 24)
    for (start <- edges; len <- Seq(0, 1, 31, 32, 511, 512, 5000); delta <- Seq(0, 1, 31, 32, 33) if delta <= len + 40) {
      checkRoundTrip(start, start + delta, start + len)
    }
  }

  @Test def derivedOperationsMatchTheUnpackedEncoding(): Unit = {
    val pos = Position.range(source, 100, 104, 120)
    val reference = new RangePosition(source, 100, 104, 120)
    assertEquals(reference.withStart(90), pos.withStart(90))
    assertEquals(reference.withPoint(110), pos.withPoint(110))
    assertEquals(reference.withEnd(130), pos.withEnd(130))
    assertEquals(reference.withShift(3), pos.withShift(3))
    assertEquals(reference.focus, pos.focus)
    assertEquals(reference.makeTransparent, pos.makeTransparent)
    assertTrue(pos.makeTransparent.isTransparent)
    val other = Position.range(source, 110, 115, 140)
    assertEquals(reference | other, pos | other)
    assertEquals(reference.overlaps(other), pos.overlaps(other))
    assertEquals(reference.includes(other), pos.includes(other))
  }

  @Test def startAfterEndIsStillRejected(): Unit = {
    var failed = false
    try Position.range(source, 10, 10, 5) catch { case _: AssertionError => failed = true }
    assertTrue(failed)
  }
}
