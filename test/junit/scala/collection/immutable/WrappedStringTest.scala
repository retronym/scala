package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class WrappedStringTest {

  @Test // scala/bug#11518
  def indexOf_nonChar(): Unit = {
    assertEquals(-1, new WrappedString("test").indexOf[Any]("not a Char")) // doesn't overflow
  }

  @Test def copyToArrayNothingToCopy(): Unit = {
    for (destLen <- List(0, 2); start <- List(-1, 0, 1, 2, 3); len <- List(-1, 0, 1, 5)) {
      val msg = s"copyToArray(new Array[Char]($destLen), $start, $len)"
      def run(xs: Seq[Char]): Any = {
        val dest = new Array[Char](destLen)
        try (xs.copyToArray(dest, start, len), dest.toList) catch { case e: IndexOutOfBoundsException => "IndexOutOfBoundsException" }
      }
      assertEquals(msg, run(Vector('a', 'b', 'c')), run(new WrappedString("abc")))
    }
  }

  @Test def startsWithOffsetOutOfRange(): Unit = {
    val s = "abc"
    for (offset <- List(Int.MinValue, -1, 0, 1, 2, 3, 4, Int.MaxValue); that <- List("", "a", "ab", "bc", "c", "abc", "abcd")) {
      val msg = s"startsWith(\"$that\", $offset)"
      assertEquals(msg, s.toList.startsWith(that.toList, offset), new WrappedString(s).startsWith(new WrappedString(that), offset))
      assertEquals(msg, s.toList.startsWith(that.toList, offset), new WrappedString(s).startsWith(that.toList, offset))
    }
  }
}
