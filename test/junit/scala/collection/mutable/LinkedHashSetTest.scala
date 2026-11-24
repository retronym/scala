package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.annotation.nowarn
import scala.collection.mutable

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashSetTest {
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

  // scala/bug#12121: `s ++= v` with a lazy view `v` of `s` should add what `v` holds before the call
  @Test
  def addAllLazyViewOfSelf(): Unit = {
    val s = mutable.LinkedHashSet(1, 2, 3)
    s ++= s.view.map(_ * 2)
    Assert.assertEquals(Set(1, 2, 3, 4, 6), s.toSet)
  }
}
