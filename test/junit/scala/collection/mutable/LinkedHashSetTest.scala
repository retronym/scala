package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.annotation.nowarn
import scala.collection.mutable

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashSetTest {
  // HashSet.clone and HashSet.from(a hash set) reuse the stored hashes and presize; copies of a LinkedHashSet call hashCode again
  @Test def `copies of a hash set reuse the stored hashes`(): Unit = {
    var hashes = 0
    final case class Key(i: Int) { override def hashCode = { hashes += 1; i } }
    val lhs = mutable.LinkedHashSet.from((0 until 1000).map(Key(_)))
    val hs = mutable.HashSet.from(lhs)
    def count(expected: Iterable[Key])(f: => mutable.LinkedHashSet[Key]): Int = {
      hashes = 0
      val copy = f
      val n = hashes
      Assert.assertEquals(expected.toList, copy.toList)
      n
    }
    val counts = List(
      "clone" -> count(lhs)(lhs.clone()),
      "from(LinkedHashSet)" -> count(lhs)(mutable.LinkedHashSet.from(lhs)),
      "from(HashSet)" -> count(hs)(mutable.LinkedHashSet.from(hs)),
    )
    Assert.assertEquals(counts.map { case (k, _) => (k, 0) }, counts)
  }

  @nowarn("msg=inheritance from class LinkedHashSet")
  class TestClass extends mutable.LinkedHashSet[String] {
    def lastItemRef = lastEntry
  }
  
  @Test
  def testClear(): Unit = {
    val lhs = new TestClass
    Seq("a", "b").foreach(k => lhs.add(k))
    
    Assert.assertNotNull(lhs.lastItemRef)
    lhs.clear()
    Assert.assertNull(lhs.lastItemRef)
  }

  @Test
  def testfromfactorymethod(): Unit = {
    val data = List(1,2,3,4,5,6,7,8)
    val lhs = new mutable.LinkedHashSet[Int]
    data.foreach(x => lhs.addOne(x))

    val fromlhs1 = LinkedHashSet.from(data)
    Assert.assertEquals(fromlhs1, lhs)

    val fromlhs2 = LinkedHashSet.from(lhs)
    Assert.assertEquals(fromlhs2, lhs)
    Assert.assertFalse(fromlhs2 eq lhs)
  }
}
