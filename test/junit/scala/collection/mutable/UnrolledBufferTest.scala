package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.assertEquals

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
}
