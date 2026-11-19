package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.{assertEquals, assertThrows, assertTrue}

import scala.util.Random

@RunWith(classOf[JUnit4])
class UnrolledBufferTestTest {
  @Test
  def test_SI9254_original(): Unit = {
    val b = new UnrolledBuffer[Int]()
    (1 to 16).foreach(i => b append i)
    b.insert(0,-1)
    b append 17
    assert(b sameElements (Seq(-1) ++ (1 to 16) ++ Seq(17)))
  }

  @Test
  def test_SI9254_additional(): Unit = {
    val b = new UnrolledBuffer[Int]()
    (1 to 100).foreach(i => b append i)
    b.insert(40, -1)
    assert(b sameElements((1 to 40) ++ Seq(-1) ++ (41 to 100)))
  }

  @Test
  def test_removal(): Unit = {
    val b = UnrolledBuffer(1, 2, 3)
    b -= 2
    assert(b == (1 :: 3 :: Nil))
    assert(b.size == 2)
  }

  private val nodeSize = 32

  @Test
  def insertAtNodeBoundary(): Unit = {
    val b = UnrolledBuffer.tabulate(2 * nodeSize)(identity)
    b.insert(nodeSize, -1)
    val expected = ((0 until nodeSize) :+ -1) ++ (nodeSize until 2 * nodeSize)
    assertEquals(expected.length, b.length)
    assertEquals(expected.toList, b.iterator.toList)
    assertEquals(expected(40), b(40))
    assertEquals(expected.last, b.last)
  }

  @Test
  def insertAllAtEndKeepsTail(): Unit = {
    val b = UnrolledBuffer.tabulate(nodeSize)(identity)
    b.insertAll(nodeSize, List(100, 101))
    b += 200
    assertEquals((0 until nodeSize).toList ::: List(100, 101, 200), b.toList)
    b.concat(UnrolledBuffer(1, 2, 3))
    assertEquals((0 until nodeSize).toList ::: List(100, 101, 200, 1, 2, 3), b.toList)
  }

  @Test
  def removeRangeValidatesBeforeMutating(): Unit = {
    val b = UnrolledBuffer.tabulate(10)(identity)
    assertThrows(classOf[IndexOutOfBoundsException], () => b.remove(5, 100))
    assertThrows(classOf[IndexOutOfBoundsException], () => b.remove(-1, 2))
    assertThrows(classOf[IllegalArgumentException], () => b.remove(5, -1))
    assertEquals(List.range(0, 10), b.toList)
    b.remove(10, 0)
    b.remove(8, 2)
    assertEquals(List.range(0, 8), b.toList)
  }

  @Test
  def patchInPlaceClamps(): Unit = {
    def check(from: Int, patch: List[Int], replaced: Int): Unit = {
      val expected = ArrayBuffer.tabulate(10)(identity).patchInPlace(from, patch, replaced)
      val actual = UnrolledBuffer.tabulate(10)(identity).patchInPlace(from, patch, replaced)
      assertEquals(s"patchInPlace($from, $patch, $replaced)", expected.toList, actual.toList)
    }
    for (from <- List(-3, 0, 5, 10, 13); patch <- List(Nil, List(-1, -2)); replaced <- List(-1, 0, 2, 5, 100))
      check(from, patch, replaced)
  }

  // Differential test against ArrayBuffer, with sizes and positions at and around multiples of the node size.
  @Test
  def randomizedAgainstArrayBuffer(): Unit = {
    val counts = Array(0, 1, 2, 5, nodeSize - 1, nodeSize, nodeSize + 1, 2 * nodeSize + 6)
    for (seed <- 0 until 200) {
      val rnd = new Random(seed)
      val u = UnrolledBuffer.empty[Int]
      val r = ArrayBuffer.empty[Int]
      val log = new ListBuffer[String]
      def position(): Int = {
        val n = r.length
        if (n == 0) 0
        else if (rnd.nextBoolean()) rnd.nextInt(n + 1)
        else math.min(n, math.max(0, rnd.nextInt(n / nodeSize + 1) * nodeSize + rnd.nextInt(3) - 1))
      }
      for (_ <- 0 until 60) {
        val n = r.length
        val i = position()
        val x = rnd.nextInt(1000)
        val cnt = counts(rnd.nextInt(counts.length))
        val xs = List.tabulate(cnt)(x + _)
        rnd.nextInt(10) match {
          case 0 | 1 => log += s"addAll($cnt)"; u ++= xs; r ++= xs
          case 2 => log += s"prepend x$cnt"; xs.foreach { e => u.prepend(e); r.prepend(e) }
          case 3 => log += s"insert($i)"; u.insert(i, x); r.insert(i, x)
          case 4 | 5 => log += s"insertAll($i, $cnt)"; u.insertAll(i, xs); r.insertAll(i, xs)
          case 6 => if (n > 0) { val j = math.min(i, n - 1); log += s"remove($j)"; assertEquals(r.remove(j), u.remove(j)) }
          case 7 => val c = rnd.nextInt(n - i + 1); log += s"remove($i, $c)"; u.remove(i, c); r.remove(i, c)
          case 8 =>
            log += s"concat($cnt)"
            val that = UnrolledBuffer.from(xs)
            u.concat(that)
            r ++= xs
            assertTrue(that.isEmpty)
          case 9 =>
            val replaced = rnd.nextInt(n + 3) - 1
            val from = i + rnd.nextInt(3) - 1
            log += s"patchInPlace($from, $cnt, $replaced)"
            u.patchInPlace(from, xs, replaced)
            r.patchInPlace(from, xs, replaced)
        }
        def msg = s"seed $seed after ${log.mkString("; ")}"
        assertEquals(msg, r.length, u.length)
        assertEquals(msg, r.toList, u.iterator.toList)
        val viaForeach = new ListBuffer[Int]
        u.foreach(viaForeach += _)
        assertEquals(msg, r.toList, viaForeach.toList)
        if (r.nonEmpty) {
          assertEquals(msg, r.last, u.last)
          assertEquals(msg, r(r.length - 1), u(u.length - 1))
          assertEquals(msg, r(r.length / 2), u(r.length / 2))
        }
      }
    }
  }
}
