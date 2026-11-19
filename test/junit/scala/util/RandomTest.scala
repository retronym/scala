package scala.util

import org.junit.{Assert, Test}

import scala.annotation.unused
import scala.collection.immutable.WrappedString

class RandomTest {
  // Test for scala/bug#9059
  @Test def testAlphanumeric(): Unit = {
    def isAlphaNum(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

    val items = Random.alphanumeric.take(100000)
    for (c <- items) {
      Assert.assertTrue(s"$c should be alphanumeric", isAlphaNum(c))
    }
  }

  // These tests check that the static type returned by `Random.shuffle` is as specific as possible
  @Test def testShuffle(): Unit = {
    val s = Random.shuffle("bar")
    @unused val sT: WrappedString = s
    val ws = Random.shuffle("bar".toSeq)
    @unused val wsT: WrappedString = ws
    val lhm = Random.shuffle(collection.mutable.LinkedHashMap("foo" -> 1, "bar" -> 2))
    @unused val lhmT: collection.mutable.LinkedHashMap[String, Int] = lhm
  }

  // `nextLong(n)` halves a large `n` on the strength of two random bits, until what is left fits
  // `nextInt`. Fix the bits drawn at each halving, take the least and the greatest Int at the end,
  // and the resulting intervals must leave no gap in `0 until n`.
  @Test def nextLongReachesAllValues(): Unit = {
    class Stub(bits: List[Int], greatest: Boolean) extends java.util.Random {
      private[this] var remaining = bits
      override def nextInt(n: Int): Int =
        if (n > 4) { if (greatest) n - 1 else 0 }
        else try remaining.head % n finally remaining = remaining.tail
    }
    val choices = for (a <- 0 until 4; b <- 0 until 4; c <- 0 until 4) yield List(a, b, c)
    def check(n: Long): Unit = {
      val intervals = choices.map { bits =>
        (new Random(new Stub(bits, greatest = false)).nextLong(n), new Random(new Stub(bits, greatest = true)).nextLong(n))
      }.sorted
      Assert.assertEquals(s"least value for n=$n", 0L, intervals.head._1)
      Assert.assertEquals(s"greatest value for n=$n", n - 1, intervals.map(_._2).max)
      intervals.foldLeft(-1L) { case (reached, (lo, hi)) =>
        Assert.assertTrue(s"nextLong($n) cannot return ${reached + 1}", lo <= reached + 1)
        reached max hi
      }
    }
    check(Int.MaxValue.toLong)
    check(Int.MaxValue.toLong + 1)
    check(Int.MaxValue.toLong + 2)
    check(2L * Int.MaxValue - 1)
    check(4L * Int.MaxValue - 3)
  }

  private class Fixed(d: Double) extends java.util.Random {
    override def nextDouble(): Double = d
    override def nextFloat(): Float = d.toFloat
  }
  private val fractions = List(0.0, 0.25, 0.5, 0.75, 1.0 - 1.0 / (1 << 24))

  // scala/bug#13098
  @Test def betweenDoubleWhenSpanOverflows(): Unit = {
    val max = Double.MaxValue
    def at(d: Double, lo: Double, hi: Double) = new Random(new Fixed(d)).between(lo, hi)
    Assert.assertArrayEquals(Array(-max, -max / 2, 0.0, max / 2), fractions.init.map(at(_, -max, max)).toArray, max * 1e-15)
    val last = at(fractions.last, -max, max)
    Assert.assertTrue(last.toString, last > max / 2 && last < max)
    Assert.assertEquals(-1e308, at(0.0, -1e308, 1e308), 0.0)
    Assert.assertEquals(5e307, at(0.75, -1e308, 1e308), 1e292)
  }

  @Test def betweenFloatWhenSpanOverflows(): Unit = {
    val max = Float.MaxValue
    def at(d: Double, lo: Float, hi: Float) = new Random(new Fixed(d)).between(lo, hi)
    Assert.assertArrayEquals(Array(-max, -max / 2, 0.0f, max / 2), fractions.init.map(at(_, -max, max)).toArray, max * 1e-6f)
    val last = at(fractions.last, -max, max)
    Assert.assertTrue(last.toString, last > max / 2 && last < max)
  }

  // The results for spans that do not overflow are those of `next * (max - min) + min`.
  @Test def betweenUnchangedWhenSpanDoesNotOverflow(): Unit = {
    val seeded = new java.util.Random(42)
    for (_ <- 1 to 1000) {
      val d = seeded.nextDouble()
      val (lo, hi) = (seeded.nextGaussian() * 1e300, seeded.nextDouble() * 7e307)
      if (lo < hi) {
        Assert.assertEquals(d * (hi - lo) + lo, new Random(new Fixed(d)).between(lo, hi), 0.0)
        val (flo, fhi, f) = ((lo / 1e270).toFloat, (hi / 1e270).toFloat, d.toFloat)
        if (flo < fhi && f * (fhi - flo) + flo < fhi)
          Assert.assertEquals(f * (fhi - flo) + flo, new Random(new Fixed(d)).between(flo, fhi), 0.0f)
      }
    }
    Assert.assertEquals(1.5, new Random(new Fixed(0.25)).between(1.0, 3.0), 0.0)
    Assert.assertEquals(java.lang.Math.nextDown(3.0), new Random(new Fixed(java.lang.Math.nextDown(1.0))).between(1.0, 3.0), 0.0)
  }
}
