package scala.collection

import org.junit.Assert.{assertThrows => _, _}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class SeqViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("SeqView(<not computed>)", Seq(1, 2, 3).view.toString)
  }

  private def seq(n: Int, maxIterate: Int = -1): Seq[Int] =
    new AbstractSeq[Int] {
      val values = 1 to n
      var remaining = maxIterate
      def iterator = {
        if (remaining == 0) throw new IllegalStateException()
        if (remaining > 0) remaining -= 1
        values.iterator
      }
      def apply(i: Int) = values(i)
      def length = values.length
    }
  private def seqk(n: Int, maxIterate: Int): Seq[Int] =
    new AbstractSeq[Int] {
      val values = 1 to n
      var remaining = maxIterate
      def iterator = {
        if (remaining == 0) throw new IllegalStateException()
        if (remaining > 0) remaining -= 1
        values.iterator
      }
      def apply(i: Int) = values(i)
      def length = values.length
      override def knownSize = length
    }

  @Test def `sum of seq view`: Unit = {
    assertEquals(10, seq(4).view.sum)
  }
  @Test def `sum of seq view iterates once`: Unit = {
    val sut = seqk(4, 1).view
    assertEquals(10, sut.sum)
    assertThrows[IllegalStateException](sut.sum)
  }

  @Test
  def `sorting a sorted view keeps the order of elements the new ordering ties`(): Unit = {
    val data      = List((1, "a"), (0, "x"), (1, "b"), (1, "c"))
    val byKey     = Ordering.by[(Int, String), Int](_._1)
    val allEqual  = Ordering.by[(Int, String), Int](_ => 0)
    val byLetter  = Ordering.by[(Int, String), String](_._2)
    for (first <- List(byKey, byKey.reverse, byLetter); second <- List(byKey, byKey.reverse, allEqual, first, first.reverse)) {
      assertEquals(data.sorted(first).sorted(second), data.view.sorted(first).sorted(second).toList)
      assertEquals(data.sorted(first).reverse.sorted(second), data.view.sorted(first).reverse.sorted(second).toList)
      val forced = data.view.sorted(first)
      forced.head
      assertEquals(data.sorted(first).sorted(second), forced.sorted(second).toList)
      assertEquals(data.sorted(first).reverse.sorted(second), forced.reverse.sorted(second).toList)
    }
  }
}
