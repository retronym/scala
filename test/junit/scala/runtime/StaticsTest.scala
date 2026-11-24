package scala.runtime

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class StaticsTest {
  // A Float converts to Double exactly, so the two must hash alike, as must a Long of the same value.
  private def check(f: Float): Unit = {
    val d = f.toDouble
    assertEquals(s"$f: Float and Double", d.##, f.##)
    assertEquals(s"$f: floatHash and anyHash", f.##, (f: Any).##)
    val l = f.toLong
    if (l.toDouble == d && l != Long.MaxValue) {
      assertTrue(s"$f == $l", (f: Any) == l)
      assertEquals(s"$f: Float and Long", l.##, f.##)
    }
  }

  @Test def floatHashAgreesWithDoubleAndLong(): Unit = {
    for (i <- 0 to 64) {
      val p = java.lang.Math.scalb(1f, i)
      for (f <- List(p, java.lang.Math.nextDown(p), java.lang.Math.nextUp(p), p * 1.5f); x <- List(f, -f))
        check(x)
    }
    List(0f, -0f, 0.5f, Float.MaxValue, Float.MinValue, Float.MinPositiveValue, Float.PositiveInfinity, Float.NegativeInfinity).foreach(check)
  }

  @Test def t8150(): Unit = {
    val f = 2147483648f
    assertEquals(2147483648L.##, f.##)
    assertEquals(2147483648.0.##, f.##)
    assertEquals(Int.MinValue.##, Int.MinValue.toFloat.##)
    assertTrue(collection.immutable.HashSet[Any](2147483648L, 1, 2, 3, 4, 5).contains(f))
    assertEquals(Some(1), collection.mutable.HashMap[Any, Int](2147483648.0 -> 1).get(f))
    assertEquals(1, List[Any](f, 2147483648.0, 2147483648L).distinct.size)
  }
}
