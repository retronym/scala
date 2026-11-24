package scala
package collection
package immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TreeSeqMapTest {
  @Test
  def t7445(): Unit = {
    val m = TreeSeqMap(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(TreeSeqMap(2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.tail)
  }
  @Test
  def testBuilder(): Unit = {
    val m = TreeSeqMap("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4")
    assertEquals(List("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4"), m.toList)
  }
  @Test
  def testHeadTailLastInitWhenOrderingByInsertion(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(3 -> 1, m.head)
    assertEquals(TreeSeqMap(2 -> 2, 1 -> 3), m.tail)
    assertEquals(1 -> 3, m.last)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2), m.init)
  }
  @Test
  def testHeadTailLastInitWhenOrderingByModification(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(TreeSeqMap.OrderBy.Modification).updated(2, 4)
    assertEquals(3 -> 1, m.head)
    assertEquals(TreeSeqMap(1 -> 3, 2 -> 4), m.tail)
    assertEquals(2 -> 4, m.last)
    assertEquals(TreeSeqMap(3 -> 1, 1 -> 3), m.init)
  }
  @Test
  def testAddWhenOrderingByInsertion(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 4, 1 -> 3), m + (2 -> 4))
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3), m + (2 -> 2))
    assertEquals(TreeSeqMap(3 -> 2, 2 -> 2, 1 -> 3), m + (3 -> 2))
  }
  @Test
  def testRemoveWhenOrderingByInsertion(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2), m - 1)
    assertEquals(TreeSeqMap(3 -> 1, 1 -> 3), m - 2)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3), m - 4)
  }
  @Test
  def testAddWhenOrderingByModification(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(TreeSeqMap.OrderBy.Modification)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(TreeSeqMap(3 -> 1, 1 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(TreeSeqMap(3 -> 1, 1 -> 3, 2 -> 2), m + (2 -> 2))
    assertEquals(TreeSeqMap(2 -> 2, 3 -> 2, 1 -> 4), m + (3 -> 2) + (1 -> 4))
  }
  @Test
  def testRemoveWhenOrderingByModification(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(TreeSeqMap.OrderBy.Modification).updated(3, 3)
    assertEquals(TreeSeqMap(2 -> 2, 3 -> 3), m - 1)
    assertEquals(TreeSeqMap(1 -> 3, 3 -> 3), m - 2)
    assertEquals(TreeSeqMap(2 -> 2, 1 -> 3, 3 -> 3), m - 4)
  }
  @Test
  def testRemoveMultipleWhenOrderingByInsertion(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(TreeSeqMap(3 -> 1, 2 -> 2, 5 -> 4), (m - 1) - 4)
    assertEquals(TreeSeqMap(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(TreeSeqMap(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }
  @Test
  def testRemoveMultipleWhenOrderingByModification(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4).orderingBy(TreeSeqMap.OrderBy.Modification).updated(3, 3)
    assertEquals(TreeSeqMap(2 -> 2, 5 -> 4, 3 -> 3), (m - 1) - 4)
    assertEquals(TreeSeqMap(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(TreeSeqMap(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }
  @Test
  def testIterator(): Unit = {
    assertEquals(Nil, TreeSeqMap.empty.iterator.toList)
    assertEquals(List(4 -> 1), TreeSeqMap(4 -> 1).iterator.toList)
    assertEquals(List(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), TreeSeqMap(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4).iterator.toList)
  }
  @Test
  def testRemoveIterator(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2, 5 -> 4), ((m - 1) - 4).iterator.toList)
    assertEquals(List(1 -> 3, 5 -> 4), ((m - 3) - 2 - 4).iterator.toList)
    assertEquals(List(4 -> 5, 5 -> 4), ((m - 3) - 1 - 2).iterator.toList)
  }
  @Test
  def testSlice(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2), ((m - 1) - 4).slice(0, 2).iterator.toList)
    assertEquals(List(5 -> 4), ((m - 3) - 2 - 4).slice(1, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(2, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(2, 2).iterator.toList)
    assertEquals(List(3 -> 3, 4 -> 4, 5 -> 5), TreeSeqMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(2, 5).iterator.toList)
    assertEquals(List(7 -> 7), TreeSeqMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(6, 7).iterator.toList)
    assertEquals(List(), TreeSeqMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(7, 7).iterator.toList)
  }

  @Test
  def testSplitAt(): Unit = {
    val m = TreeSeqMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    var t = m.splitAt(0)
    assertEquals((List(), List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(1)
    assertEquals((List(3 -> 1), List(2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(2)
    assertEquals((List(3 -> 1, 2 -> 2), List(1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(3)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3), List(4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(4)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5), List(5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(5)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4), List()), (t._1.iterator.toList, t._2.iterator.toList))
  }

  @Test
  def testConcatAsSeqs(): Unit = {
    val x = TreeSeqMap(3L -> "wish")
    val y = TreeSeqMap(9L -> "nine", 3L -> "three", 7L -> "seven", -1L -> "negative", 42L -> "Adams", 6L -> "vi")
    val xy = x ++ y
    val xySeq = x.toSeq ++ y.toSeq
    val xyMap = xySeq.to(TreeSeqMap)
    assertEquals(xy.toList, xyMap.toList)
  }

  @Test
  def testAppend(): Unit = {
    import TreeSeqMap._
    import TreeSeqMap.Ordering._

    for (i <- 1 until 1027) {
      val o1 = Ordering((1 until i).map(n => n -> toBinaryString(n)): _*).exclude(i / 2).exclude(i / 10).exclude(i - 1)
      val o2 = Ordering((1 until i).map(n => n -> toBinaryString(n)): _*).exclude(i / 2).exclude(i / 10).exclude(i - 1)
      val v = toBinaryString(i)
      val o3 = o1.include(i, v)
      val o4 = o1.append(i, v)
      assertEquals(s"$i", o3, o4)
      val o5 = o2.appendInPlace(i, v)
      assertEquals(s"$i", o3, o5)
    }
  }
  @Test
  def testEmpty(): Unit = {
    {
      val e1 = TreeSeqMap.empty[Int, Int]
      val e2 = e1 + (3 -> 1) + (2 -> 2) + (1 -> 3) + (3 -> 4)
      val e3 = e2.tail
      assertEquals(s"default empty keeps insertion order", List(2 -> 2, 1 -> 3), e3.toList)
    }
    {
      val e1 = TreeSeqMap.empty[Int, Int](TreeSeqMap.OrderBy.Modification)
      val e2 = e1 + (3 -> 1) + (2 -> 2) + (1 -> 3) + (3 -> 4)
      val e3 = e2.tail
      assertEquals(s"modification empty keeps modification order", List(1 -> 3, 3 -> 4), e3.toList)
    }
    {
      val e1 = TreeSeqMap(3 -> 1).empty
      val e2 = e1 + (3 -> 1) + (2 -> 2) + (1 -> 3) + (3 -> 4)
      val e3 = e2.tail
      assertEquals(s"default empty from instance keeps insertion order", List(2 -> 2, 1 -> 3), e3.toList)
    }
    {
      val e1 = TreeSeqMap(3 -> 1).orderingBy(TreeSeqMap.OrderBy.Modification).empty
      val e2 = e1 + (3 -> 1) + (2 -> 2) + (1 -> 3) + (3 -> 4)
      val e3 = e2.tail
      assertEquals(s"modification empty from instance keeps modification order", List(1 -> 3, 3 -> 4), e3.toList)
    }
  }
  @Test
  def `t13019 empty.iterator is idempotent`: Unit = {
    val m = util.Try(TreeSeqMap.empty.iterator.next())
    assertTrue(m.isFailure)
    assertFalse("empty iterator does not have next", TreeSeqMap.empty.iterator.hasNext)
  }

  @Test
  def builderReuseAfterClear(): Unit = {
    val b = TreeSeqMap.newBuilder[Int, String]
    (1 to 5).foreach(i => b += (i -> "x"))
    b.result()
    b.clear()
    b += (1 -> "a")
    b += (1 -> "b")
    val m = b.result()
    assertEquals(1, m.size)
    assertEquals(1, m.iterator.size)
    assertEquals(List(1 -> "b"), m.toList)
    assertTrue(m.removed(1).isEmpty)
  }

  @Test
  def refreshKeepsMappingInSync(): Unit = {
    for (orderBy <- List(TreeSeqMap.OrderBy.Insertion, TreeSeqMap.OrderBy.Modification)) {
      val r = (TreeSeqMap.empty[Int, String](orderBy) ++ List(1 -> "a", 2 -> "b", 3 -> "c")).refresh(1)
      assertEquals(List(2 -> "b", 3 -> "c", 1 -> "a"), r.toList)
      assertEquals(List(2 -> "b", 3 -> "c"), r.removed(1).toList)
      assertEquals(List(2 -> "b", 3 -> "c", 1 -> "a"), r.refresh(1).toList)
      assertEquals(List(3 -> "c", 1 -> "a", 2 -> "b"), r.refresh(2).toList)
      assertEquals(List(2 -> "b", 3 -> "c", 1 -> "A"), r.updated(1, "A").toList)
      assertEquals(List(2 -> "b", 3 -> "c", 1 -> "A"), (r ++ List(1 -> "A")).toList)
      assertEquals(List(3 -> "c", 1 -> "a"), r.tail.toList)
      assertEquals(List(2 -> "b", 3 -> "c"), r.init.toList)
      assertEquals(r.orderedBy, orderBy)
    }
  }

  // Differential test of refresh mixed with other operations, against an association vector.
  @Test
  def refreshRandomized(): Unit = {
    type Model = Vector[(Int, String)]
    for (seed <- 0 until 200; orderBy <- List(TreeSeqMap.OrderBy.Insertion, TreeSeqMap.OrderBy.Modification)) {
      val byModification = orderBy == TreeSeqMap.OrderBy.Modification
      def update(m: Model, k: Int, v: String): Model = m.indexWhere(_._1 == k) match {
        case -1                  => m :+ (k -> v)
        case i if byModification => m.patch(i, Nil, 1) :+ (k -> v)
        case i                   => m.updated(i, k -> v)
      }
      def refresh(m: Model, k: Int): Model = m.indexWhere(_._1 == k) match {
        case -1 => m
        case i  => m.patch(i, Nil, 1) :+ m(i)
      }
      val rnd = new scala.util.Random(seed)
      var t = TreeSeqMap.empty[Int, String](orderBy)
      var r: Model = Vector.empty
      val log = List.newBuilder[String]
      for (_ <- 0 until 40) {
        val k = rnd.nextInt(8)
        val v = "v" + rnd.nextInt(3)
        rnd.nextInt(10) match {
          case 0 | 1 | 2 => log += s"updated($k, $v)"; t = t.updated(k, v); r = update(r, k, v)
          case 3 | 4 | 5 => log += s"refresh($k)"; t = t.refresh(k); r = refresh(r, k)
          case 6 => log += s"removed($k)"; t = t.removed(k); r = r.filter(_._1 != k)
          case 7 =>
            val xs = List.fill(rnd.nextInt(4))(rnd.nextInt(8) -> ("c" + rnd.nextInt(2)))
            log += s"concat($xs)"
            t = t ++ xs
            xs.foreach { case (k, v) => r = update(r, k, v) }
          case 8 => if (r.nonEmpty) { log += "tail"; t = t.tail; r = r.tail }
          case 9 => if (r.nonEmpty) { log += "init"; t = t.init; r = r.init }
        }
        def msg = s"seed $seed, $orderBy, after ${log.result().mkString("; ")}"
        assertEquals(msg, r, t.toVector)
        assertEquals(msg, r.size, t.size)
        assertEquals(msg, r.toMap, t)
        assertEquals(msg, r.headOption, t.headOption)
        assertEquals(msg, r.lastOption, t.lastOption)
      }
    }
  }
}
object TreeSeqMapTest extends App {
  import TreeSeqMap.Ordering._

  val o0 = TreeSeqMap.Ordering(1 -> toBinaryString(1), 2 -> toBinaryString(2), -1 -> toBinaryString(-1), 5 -> toBinaryString(5), 100 -> toBinaryString(100))
  println(o0)
  println()
  var o = TreeSeqMap.Ordering.empty[String]
  for (i <- 1 until 129) {
    val v = toBinaryString(i)
    o = o.include(i, v)
    println(s"$i [$v]:\n" + o)
    println()
  }
}
