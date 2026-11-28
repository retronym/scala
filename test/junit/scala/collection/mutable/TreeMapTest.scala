package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.{immutable, mutable}
import scala.util.chaining._

@RunWith(classOf[JUnit4])
class TreeMapTest {
  private var compares = 0
  private val ord: Ordering[Int] = (x, y) => { compares += 1; Integer.compare(x, y) }
  private def count[T](f: => T): (T, Int) = { compares = 0; val r = f; (r, compares) }
  private val entries = List.tabulate(1000)(i => i -> i.toString)

  @Test def `from of a sorted map with the same ordering builds the tree without comparisons`(): Unit = {
    val mtm = mutable.TreeMap.from(entries)(ord)
    val itm = immutable.TreeMap.from(entries)(ord)
    for (src <- List[collection.SortedMap[Int, String]](mtm, itm)) {
      val (copy, c) = count(mutable.TreeMap.from(src)(ord))
      assertEquals((entries, 0), (copy.toList, c))
      copy(-1) = "new"
      assertEquals(entries.size, src.size)
    }
    // a range projection copies only its range
    assertEquals(entries.slice(10, 20), mutable.TreeMap.from(mtm.range(10, 20))(ord).toList)
    // another ordering inserts
    assertEquals(entries.reverse, mutable.TreeMap.from(mtm)(Ordering.Int.reverse).toList)
  }

  @Ignore("needs an override of clone in mutable.TreeMap and mutable.TreeSet, which is not forward binary compatible")
  @Test def `clone copies the tree without comparisons`(): Unit = {
    val mtm = mutable.TreeMap.from(entries)(ord)
    assertEquals((entries, 0), count(mtm.clone()).pipe { case (m, c) => (m.toList, c) })
    val mts = mutable.TreeSet.from(entries.map(_._1))(ord)
    assertEquals((entries.map(_._1), 0), count(mts.clone()).pipe { case (s, c) => (s.toList, c) })
  }


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

}
