package scala.reflect.internal.util

import org.junit.Assert.{ assertThrows => _, _ }
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil._

@RunWith(classOf[JUnit4])
class SourceFileTest {
  def lineContentOf(code: String, offset: Int) =
    Position.offset(new BatchSourceFile("", code), offset).lineContent

  @Test
  def si8205_overflow(): Unit = {
    val file = new BatchSourceFile("", "code no newline")
    // the bug in lineToString counted until MaxValue, and the AIOOBE came from here
    assertFalse(file.isEndOfLine(Int.MaxValue))
  }

  @Test def si8630_lineToString(): Unit = {
    val code = "abc "
    assertEquals(code, new BatchSourceFile("", code).lineToString(0))
  }

  @Test
  def si8205_lineToString(): Unit = {
    assertEquals("", lineContentOf("", 0))
    assertEquals("abc", lineContentOf("abc", 0))
    assertEquals("abc", lineContentOf("abc", 3))
    assertEquals("code no newline", lineContentOf("code no newline", 1))
    assertEquals("", lineContentOf("\n", 0))
    assertEquals("abc", lineContentOf("abc\ndef", 0))
    assertEquals("abc", lineContentOf("abc\ndef", 3))
    assertEquals("def", lineContentOf("abc\ndef", 4))
    assertEquals("def", lineContentOf("abc\ndef", 6))
    assertEquals("def", lineContentOf("abc\ndef\n", 7))
  }

  @Test
  def CRisEOL(): Unit = {
    assertEquals("", lineContentOf("\r", 0))
    assertEquals("abc", lineContentOf("abc\rdef", 0))
    assertEquals("abc", lineContentOf("abc\rdef", 3))
    assertEquals("def", lineContentOf("abc\rdef", 4))
    assertEquals("def", lineContentOf("abc\rdef", 6))
    assertEquals("def", lineContentOf("abc\rdef\r", 7))
  }

  @Test
  def CRNLisEOL(): Unit = {
    assertEquals("", lineContentOf("\r\n", 0))
    assertEquals("abc", lineContentOf("abc\r\ndef", 0))
    assertEquals("abc", lineContentOf("abc\r\ndef", 3))
    assertEquals("abc", lineContentOf("abc\r\ndef", 4))
    assertEquals("def", lineContentOf("abc\r\ndef", 5))
    assertEquals("def", lineContentOf("abc\r\ndef", 7))
    assertEquals("def", lineContentOf("abc\r\ndef", 8))
    assertEquals("def", lineContentOf("abc\r\ndef\r\n", 9))
  }

  @Test def `t9885 lineToOffset throws on bad line`: Unit = {
    val text = "a\nb\nc\n"
    val f = new BatchSourceFile("batch", text)
    // EOL is line terminator, not line separator, so there is not an empty 4th line
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))

    // Position and SourceFile count differently
    val p = Position.offset(f, text.length - 1)
    val q = Position.offset(f, f.lineToOffset(p.line - 1))
    assertEquals(p.line, 3)
    assertEquals(p.line, q.line)
    assertEquals(p.column, q.column + 1)
    assertThrows[IndexOutOfBoundsException] {
      Position.offset(f, f.lineToOffset(p.line))
    }
  }
  @Test def `t9885 lineToOffset ignores lack of final EOL`: Unit = {
    val text = "a\nb\nc"
    val f = new BatchSourceFile("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))
    // final EOL is appended silently; this could throw OOB
    assertEquals(2, f.offsetToLine(text.length))
  }
  @Test def `t11572 offsetToLine throws on bad offset`: Unit = {
    val text = "a\nb\nc\n"
    val f = new BatchSourceFile("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(6)
    }
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(-1)
    }
    assertEquals(0, f.offsetToLine(0))
    assertEquals(0, f.offsetToLine(1))
    assertEquals(1, f.offsetToLine(2))
    assertEquals(2, f.offsetToLine(4))
    assertEquals(2, f.offsetToLine(5))
  }

  private def linesOf(code: String) = new BatchSourceFile("", code).lines().toList

  @Test def `lines omits line terminators`(): Unit = {
    assertEquals(List("a", "b"), linesOf("a\nb\n"))
    assertEquals(List("a", "b"), linesOf("a\r\nb\r\n"))
    assertEquals(List("a", "b"), linesOf("a\rb\r"))
    assertEquals(List("a", "", "b"), linesOf("a\n\nb\n"))
    assertEquals(List("a", "", "b"), linesOf("a\r\n\r\nb\r\n"))
    assertEquals(List("a", "", "b"), linesOf("a\r\rb"))
    assertEquals(List("a", "b"), linesOf("a\nb"))
    assertEquals(List("a", "b"), linesOf("a\r\nb"))
    // form feed does not end a line, here or in lineToString
    assertEquals(List("a\fb"), linesOf("a\fb\n"))
    assertEquals(List("a\fb", "c"), linesOf("a\fb\r\nc"))
  }

  @Test def `lines has no line after the last terminator`(): Unit = {
    assertEquals(List(""), linesOf(""))
    assertEquals(List(""), linesOf("\n"))
    assertEquals(List(""), linesOf("\r\n"))
    assertEquals(List("", ""), linesOf("\n\n"))
    assertEquals(List("a"), linesOf("a"))
    assertEquals(List("a"), linesOf("a\n"))
    // trailing whitespace that is not a line terminator belongs to the last line
    assertEquals(List("a "), linesOf("a "))
    assertEquals(List("a", " "), linesOf("a\n "))
  }

  @Test def `lines agrees with lineToString`(): Unit = {
    for (code <- List("", "\n", "a", "a\n", "a\nb", "a\r\nb\r\n", "a\rb\r", "a\fb\n", "a \n\n b ", "\r\n\r\n")) {
      val f = new BatchSourceFile("", code)
      val lines = f.lines().toList
      assertEquals(code, lines, lines.indices.map(f.lineToString).toList)
      assertThrows[IndexOutOfBoundsException](f.lineToOffset(lines.length))
      assertEquals(code, lines.length - 1, f.offsetToLine(f.length - 1))
    }
  }

  @Test def `lines clips its bounds`(): Unit = {
    val f = new BatchSourceFile("", "a\r\nb\r\nc\r\n")
    assertEquals(List("b"), f.lines(1, 2).toList)
    assertEquals(List("b", "c"), f.lines(1).toList)
    assertEquals(List("a", "b", "c"), f.lines(-5, 100).toList)
    assertEquals(Nil, f.lines(3, 100).toList)
    assertEquals(Nil, f.lines(2, 1).toList)
  }

  @Test def `skipWhitespace stops at the end of the file`(): Unit = {
    val f = new BatchSourceFile("", "ab  ")
    assertEquals(0, f.skipWhitespace(0))
    assertEquals(4, f.skipWhitespace(2))
    assertEquals(4, f.skipWhitespace(4))
    val g = new BatchSourceFile("", "ab")
    assertEquals(g.length, g.skipWhitespace(2))
    val e = new BatchSourceFile("", "")
    assertEquals(e.length, e.skipWhitespace(0))
  }
}
