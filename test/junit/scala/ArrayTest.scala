package scala

import org.junit.Assert.{ assertArrayEquals, assertEquals, assertFalse, assertTrue }
import org.junit.Test

import scala.runtime.BoxedUnit
import scala.tools.testkit.AssertUtil._
import scala.util.chaining._

class ArrayTest {
  @Test
  def testArrayCopyOfUnit(): Unit = {
    val expected = new Array[BoxedUnit](32).asInstanceOf[Array[AnyRef]]
                     .tap(array => java.util.Arrays.fill(array, (): Any))
    assertArrayEquals(expected, Array.copyOf(Array[Unit](), 32).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(expected, Array.copyAs[Unit](Array[Nothing](), 32).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(expected, Array.copyAs[Unit](Array[Unit](), 32).asInstanceOf[Array[AnyRef]])
  }

  @Test
  def testArrayIsEmpty(): Unit = {
    assertTrue(Array[Int]().isEmpty)
    assertTrue(Array[Char]().isEmpty) // scala/bug#12172
    assertTrue(Array[String]().isEmpty)

    assertFalse(Array(1).isEmpty)
    assertFalse(Array[Char](1).isEmpty)
    assertFalse(Array("").isEmpty)

    def ge[T](a: Array[T]) = a.isEmpty

    assertTrue(ge(Array[Int]()))
    assertTrue(ge(Array[Char]()))
    assertTrue(ge(Array[String]()))

    assertFalse(ge(Array(1)))
    assertFalse(ge(Array[Char]('x')))
    assertFalse(ge(Array("")))
  }

  /** Test Array.equals (sameElements) for reference arrays. */
  @Test def testArraySemiDeepEquality(): Unit = {
    val xs = Array(List(1, 2, 3))
    val ys = Array(Vector(1, 2, 3))
    def anyxs = xs.asInstanceOf[Array[AnyRef]]
    def anyys = ys.asInstanceOf[Array[AnyRef]]
    assertTrue("Arrays of List and Vector should compare equal", Array.equals(anyxs, anyys))
    assertTrue(xs.sameElements(ys))     // for fun
    //assertTrue(Array.equals(xs, ys))  // would be nice
    assertTrue("Arrays of String", Array.equals(Array[AnyRef]("hello, world"), Array[AnyRef]("hello, world")))
    assertFalse("Arrays of String", Array.equals(Array[AnyRef]("hello, world"), Array[AnyRef]("goodbye, cruel world")))
  }

  @Test def `i24877 range throws on naive step`: Unit = {
    assertSameElements(Array[Int](27), Array.range(27, 42, Int.MaxValue))
  }

  @Test def copyAsAcrossElementTypes(): Unit = {
    // truncating
    assertArrayEquals(Array[AnyRef](Int.box(1), Int.box(2)), Array.copyAs[Any](Array(1, 2, 3), 2).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(Array(1, 2), Array.copyAs[Int](Array[Any](1, 2, 3), 2))
    assertArrayEquals(Array[AnyRef]("a"), Array.copyAs[String](Array[AnyRef]("a", "b"), 1).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(Array[AnyRef](), Array.copyAs[Any](Array(1, 2, 3), 0).asInstanceOf[Array[AnyRef]])
    // same length
    assertArrayEquals(Array[AnyRef](Int.box(1), Int.box(2), Int.box(3)), Array.copyAs[Any](Array(1, 2, 3), 3).asInstanceOf[Array[AnyRef]])
    // padding
    assertArrayEquals(Array[AnyRef](Int.box(1), Int.box(2), null), Array.copyAs[Any](Array(1, 2), 3).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(Array(1, 2, 0), Array.copyAs[Int](Array[Any](1, 2), 3))
    assertArrayEquals(Array[AnyRef]("a", "b", null), Array.copyAs[String](Array[AnyRef]("a", "b"), 3).asInstanceOf[Array[AnyRef]])
    assertEquals(classOf[String], Array.copyAs[String](Array[AnyRef]("a", "b"), 1).getClass.getComponentType)
  }
}
