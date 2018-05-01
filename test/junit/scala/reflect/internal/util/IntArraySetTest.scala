package scala.reflect.internal.util

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.{Assert, Test}

class IntArraySetTest {
  @Test
  def testAddRemove(): Unit = {
    val set = new IntArraySet
    assertEquals(0, set.size)
    assertFalse(set.contains(42))
    set.+=(42)
    assertTrue(set.contains(42))
    (-32 to 32).foreach(set += _)
    assertTrue(set.contains(42))
    (-32 to 32).foreach(set -= _)
    assertTrue(set.contains(42))
    set -= 42
    assertFalse(set.contains(42))
  }

  @Test
  def string(): Unit = {
    val set = new IntArraySet()
    assertEquals("IntArraySet()", set.toString)
    set += 1
    assertEquals("IntArraySet(1)", set.toString)
    set += 2
    assertEquals("IntArraySet(1, 2)", set.toString)
  }

  @Test
  def eqHash(): Unit = {
    val set1, set2 = new IntArraySet()
    assertEquals(set1, set2)
    assertEquals(set1.hashCode, set2.hashCode)
    set1 += 42
    assertFalse(set1.equals(set2))
    set1 -= 42
    assertEquals((set1, set2).toString(), set1, set2)
    assertEquals(set1.hashCode, set2.hashCode)
    set1 += 0
    set2 += 1
    assertFalse(set1.equals(set2))
    set2 += 0
    set1 += 1
    assertEquals(set1, set2)
    assertEquals(set1.hashCode, set2.hashCode)
  }
}
