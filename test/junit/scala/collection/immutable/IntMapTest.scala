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
import scala.tools.testkit.AssertUtil.assertThrows

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
  def exhaustedIterator(): Unit = {
    val m = IntMap(1 -> "a", 2 -> "b")
    for (it <- List[Iterator[Any]](m.iterator, m.keysIterator, m.valuesIterator)) {
      it.foreach(_ => ())
      for (_ <- 1 to 2) {
        assertFalse(it.hasNext)
        assertThrows[NoSuchElementException](it.next())
      }
    }
  }
}
