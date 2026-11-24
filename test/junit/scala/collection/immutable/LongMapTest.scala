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
import scala.tools.testkit.AssertUtil.assertThrows

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
  def exhaustedIterator(): Unit = {
    val m = LongMap(1L -> "a", 2L -> "b")
    for (it <- List[Iterator[Any]](m.iterator, m.keysIterator, m.valuesIterator)) {
      it.foreach(_ => ())
      for (_ <- 1 to 2) {
        assertFalse(it.hasNext)
        assertThrows[NoSuchElementException](it.next())
      }
    }
  }
}
