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

import scala.tools.testkit.ReflectUtil.{getFieldAccessible => f}

@RunWith(classOf[JUnit4])
class LongMapTest {

  @Test
  def `isEmpty O(1)`(): Unit = {
    val m = LongMap(1L -> (), 2L -> (), 3L -> ())
    f[LongMap.Bin[_]]("left").set(m, null)
    f[LongMap.Bin[_]]("right").set(m, null)
    assertFalse(m.isEmpty) // no NPE, does not access left or right
  }

  @Test
  def applyOfMissingKeyThrowsNoSuchElementException(): Unit = {
    import scala.tools.testkit.AssertUtil.assertThrows
    val m = LongMap(1L -> "a", 2L -> "b", 17L -> "c")
    assertEquals("b", m(2L))
    // same exception and message as other maps
    assertThrows[NoSuchElementException](m(3L), _ == "key not found: 3")
    assertThrows[NoSuchElementException](LongMap(1L -> "a")(3L), _ == "key not found: 3")
    assertThrows[NoSuchElementException](LongMap.empty[String](3L), _ == "key not found: 3")
    assertEquals("default", m.withDefaultValue("default")(3L))
  }

  @Test
  def removedOfAbsentKeyReturnsThis(): Unit = {
    val keys = List[Long](0, 1, 2, 3, 8, 9, 16, 255, 256, -1, -2, Long.MinValue, Long.MaxValue)
    val m = LongMap.from(keys.map(k => k -> k.toString))
    for (absent <- List[Long](4, 5, 10, 17, 254, 257, 1024, -3, Long.MinValue + 1, Long.MaxValue - 1)) {
      assertFalse(m.contains(absent))
      assertSame(s"removed($absent)", m, m.removed(absent))
      assertSame(s"- $absent", m, m - absent)
    }
    assertSame(m, m.removedAll(List[Long](4, 5, 10)))
    // removing a present key still shares the untouched subtrees
    var cur = m
    for (k <- keys) {
      val next = cur.removed(k)
      assertEquals(cur.size - 1, next.size)
      assertFalse(next.contains(k))
      assertEquals(cur.toMap - k, next.toMap)
      cur = next
    }
    assertSame(LongMap.empty, cur)
  }
}
