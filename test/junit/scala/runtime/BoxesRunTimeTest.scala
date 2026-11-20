package scala.runtime

import org.junit.Assert.assertEquals
import org.junit.Test

class BoxesRunTimeTest {
  private def check(expected: Boolean, a: Any, b: Any): Unit = {
    val (x, y) = (a.asInstanceOf[AnyRef], b.asInstanceOf[AnyRef])
    assertEquals(s"testEqual($a, $b)", expected, BoxesRunTime.testEqual(x, y))
    assertEquals(s"testNotEqual($a, $b)", !expected, BoxesRunTime.testNotEqual(x, y))
  }

  @Test def testEqualComparesValues(): Unit = {
    check(true, 1, 1)
    check(true, 1000, 1000) // outside the Integer cache
    check(false, 1000, 1001)
    check(true, 1L << 40, 1L << 40)
    check(true, 1.5, 1.5)
    check(true, 1.5f, 1.5)
    check(true, 1, 1L)
    check(true, 1000, 1000.0)
    check(true, 'a', 97)
    check(true, 'a', 'a')
    check(false, 'a', 'b')
    check(true, true, true)
    check(false, true, false)
    check(false, Double.NaN, Double.NaN)
    check(true, 0.0, -0.0)
  }
}
