/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.ReflectUtil

@RunWith(classOf[JUnit4])
class IntMapTest {
  @Test
  def `isEmpty O(1)`(): Unit = {
    val m = IntMap(1 -> (), 2 -> (), 3 -> ())
    ReflectUtil.getFieldAccessible[IntMap.Bin[_]]("left").set(m, null)
    ReflectUtil.getFieldAccessible[IntMap.Bin[_]]("right").set(m, null)
    assertFalse(m.isEmpty) // no NPE, does not access left or right
  }

  @Test
  def applyOfMissingKeyThrowsNoSuchElementException(): Unit = {
    import scala.tools.testkit.AssertUtil.assertThrows
    val m = IntMap(1 -> "a", 2 -> "b", 17 -> "c")
    assertEquals("b", m(2))
    // same exception and message as other maps
    assertThrows[NoSuchElementException](m(3), _ == "key not found: 3")
    assertThrows[NoSuchElementException](IntMap(1 -> "a")(3), _ == "key not found: 3")
    assertThrows[NoSuchElementException](IntMap.empty[String](3), _ == "key not found: 3")
    assertEquals("default", m.withDefaultValue("default")(3))
  }

  @Test
  def removedOfAbsentKeyReturnsThis(): Unit = {
    val keys = List[Int](0, 1, 2, 3, 8, 9, 16, 255, 256, -1, -2, Int.MinValue, Int.MaxValue)
    val m = IntMap.from(keys.map(k => k -> k.toString))
    for (absent <- List[Int](4, 5, 10, 17, 254, 257, 1024, -3, Int.MinValue + 1, Int.MaxValue - 1)) {
      assertFalse(m.contains(absent))
      assertSame(s"removed($absent)", m, m.removed(absent))
      assertSame(s"- $absent", m, m - absent)
    }
    assertSame(m, m.removedAll(List[Int](4, 5, 10)))
    // removing a present key still shares the untouched subtrees
    var cur = m
    for (k <- keys) {
      val next = cur.removed(k)
      assertEquals(cur.size - 1, next.size)
      assertFalse(next.contains(k))
      assertEquals(cur.toMap - k, next.toMap)
      cur = next
    }
    assertSame(IntMap.empty, cur)
  }
}
