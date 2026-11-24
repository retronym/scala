package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable

@RunWith(classOf[JUnit4])
class TreeMapTest {

  @Test
  def rangeMkString(): Unit = {

    val map = mutable.TreeMap[String, String]()

    List("a", "b", "c", "d").foreach(v => map.put(v, v))

    val range = map.range("b", "c")

    val valuesRange = range.values
    val keysRange = range.keys

    assertEquals(1, valuesRange.size)
    assertEquals(1, keysRange.size)

    assertEquals("b", valuesRange.mkString(","))
    assertEquals("b", keysRange.mkString(","))
    assertEquals("b -> b", range.mkString(","))

  }

  // scala/bug#12121: `m ++= it` with an iterator `it` over `m` should add what `it` yields before the call
  @Test
  def addAllIteratorOfSelf(): Unit = {
    val m = mutable.TreeMap(1 -> 0, 2 -> 0, 3 -> 0)
    m ++= m.keysIterator.map(k => (k * 2, 0))
    assertEquals(Set(1, 2, 3, 4, 6), m.keySet.toSet)
  }
}
