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
class LongMapTest extends scala.tools.testkit.AllocationTest {

  @Test
  def `isEmpty O(1)`(): Unit = {
    val m = LongMap(1L -> (), 2L -> (), 3L -> ())
    f[LongMap.Bin[_]]("left").set(m, null)
    f[LongMap.Bin[_]]("right").set(m, null)
    assertFalse(m.isEmpty) // no NPE, does not access left or right
  }

  // A branch keeps prefix and mask in one word: prefix has the mask bit and all bits below it clear.
  @Test
  def binFields(): Unit = {
    val fields = classOf[LongMap.Bin[_]].getDeclaredFields.toList.filterNot(f => java.lang.reflect.Modifier.isStatic(f.getModifiers))
    assertEquals(List("left: LongMap", "prefixMask: long", "right: LongMap"), fields.map(f => s"${f.getName}: ${f.getType.getSimpleName}").sorted)
  }

  @Test
  def binExtractorDoesNotAllocate(): Unit = {
    val m = LongMap((0 until 100).map(i => i.toLong -> i.toString): _*)
    val f: (Long, String) => String = (_, v) => v
    assertSame(m, nonAllocating(m.transform(f)))
  }

  @Test
  def extremeKeys(): Unit = {
    val ks = List[Long](Long.MinValue, Long.MinValue + 1, -(1L << 40), -2, -1, 0, 1, 2, 3, 1L << 62, Long.MaxValue - 1, Long.MaxValue)
    val m = LongMap(ks.map(k => k -> k.toString): _*)
    for (k <- ks) assertEquals(Some(k.toString), m.get(k))
    assertEquals(ks.sorted, m.keys.toList.sorted)
    val evens = LongMap(ks.filter(_ % 2 == 0).map(k => k -> "e"): _*)
    assertEquals(ks.toSet, m.unionWith[String](evens, (_, a, _) => a).keySet)
    assertEquals(ks.filter(_ % 2 == 0).toSet, m.intersectionWith[String, String](evens, (_, a, _) => a).keySet)
    assertEquals(ks.toSet - ks.head, (m - ks.head).keySet)
  }
}
