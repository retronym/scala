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
}
