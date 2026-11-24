package scala.collection

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.immutable.Vector

@RunWith(classOf[JUnit4])
class StrictOptimizedSeqTest {

  @Test
  def hasCorrectDistinct(): Unit = {
    assertEquals(Vector(1, 2, 3, 4, 5), Vector(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
  }

  @Test
  def hasCorrectDistinctBy(): Unit = {
    val result = Vector("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

    assertEquals(Vector("a", "aa", "aaa", "bbbb"), result)
  }

  @Test
  def padToLenNearMinValue(): Unit = {
    assertEquals(List(1), List(1).padTo(Int.MinValue, 0))
    assertEquals(Vector(1, 2, 3), Vector(1, 2, 3).padTo(Int.MinValue + 1, 0))
    assertEquals(immutable.ArraySeq(1, 2), immutable.ArraySeq(1, 2).padTo(Int.MinValue, 0))
    assertEquals(List(1, 0, 0), List(1).padTo(3, 0))
  }
}
