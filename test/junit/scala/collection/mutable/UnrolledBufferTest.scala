package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

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

  @Test
  def insertAllSelf(): Unit = {
    import org.junit.Assert.assertEquals
    // sizes within one node: insertion at a node boundary is a separate bug
    for (n <- List(0, 1, 2, 3, 15); idx <- 0 to n) {
      val expected = ArrayBuffer.range(0, n)
      expected.insertAll(idx, expected)
      val b = UnrolledBuffer.range(0, n)
      b.insertAll(idx, b)
      assertEquals(s"insertAll($idx, self) on $n elements", expected, b)
    }
    val b = UnrolledBuffer(1, 2, 3)
    b.prependAll(b)
    assertEquals(List(1, 2, 3, 1, 2, 3), b.toList)
    val c = UnrolledBuffer(1, 2, 3)
    c.patchInPlace(1, c, 1)
    assertEquals(List(1, 1, 2, 3, 3), c.toList)
  }
}
