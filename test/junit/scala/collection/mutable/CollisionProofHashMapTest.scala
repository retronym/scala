package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CollisionProofHashMapTest {

  @Test
  def getOrElseUpdate_mutationInCallback(): Unit = {
    val hm = new mutable.CollisionProofHashMap[String, String]()
    // add enough elements to resize the hash table in the callback
    def add() = 1 to 100000 foreach (i => hm(i.toString) = "callback")
    hm.getOrElseUpdate("0", {
      add()
      ""
    })
    assertEquals(Some(""), hm.get("0"))
  }

  @Test
  def getOrElseUpdate_evalOnce(): Unit = {
    var i = 0
    val hm = new mutable.CollisionProofHashMap[Int, Int]()
    hm.getOrElseUpdate(0, {i += 1; i})
    assertEquals(1, hm(0))
  }

  @Test
  def getOrElseUpdate_noEval(): Unit = {
    val hm = new mutable.CollisionProofHashMap[Int, Int]()
    hm.put(0, 0)
    hm.getOrElseUpdate(0, throw new AssertionError())
  }

  def getOrElseUpdate_keyIdempotence(): Unit = {
    val map = mutable.CollisionProofHashMap[String, String]()

    val key = "key"
    map.getOrElseUpdate(key, {
      map.getOrElseUpdate(key, "value1")
      "value2"
    })

    assertEquals(List((key, "value2")), map.toList)
  }

  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = mutable.CollisionProofHashMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("")

    assertEquals(m2(1), "a")
    assertEquals(m2(3), "")

    m2 += (3 -> "c")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "")

    m2 ++= List(4 -> "d", 5 -> "e", 6 -> "f")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "d")
    assertEquals(m2(5), "e")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    m2 --= List(3, 4, 5)
    assertEquals(m2(3), "")
    assertEquals(m2(4), "")
    assertEquals(m2(5), "")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    val m3 = m2 ++ List(3 -> "333")
    assertEquals(m2(3), "")
    assertEquals(m3(3), "333")
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testWithDefault(): Unit = {
    val m1 = mutable.CollisionProofHashMap(1 -> "a", 2 -> "b")

    val m2: mutable.Map[Int, String] = m1.withDefault(i => (i + 1).toString)
    m2.update(1, "aa")
    m2.update(100, "bb")
    m2.addAll(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: mutable.Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: mutable.Map[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testPreserveType(): Unit = {
    val m1 = mutable.CollisionProofHashMap(1 -> "a", 2 -> "b")
    val m2 = (m1 map { case (k, v) => k -> (v + "!") }).addOne(3 -> "c")
    assertEquals(m2(3), "c")

    val m3 = (m1 flatMap { case (k, v) => List(k -> (v + "!"), -k -> (v + "!")) }).addOne(3 -> "c")
    assertEquals(m3(3), "c")

    val m4 = (m1 collect { case (k, v) if k == 1 => k -> (v + "!") }).addOne(3 -> "c")
    assertEquals(m4(3), "c")

    val m5 = (m1 concat List(4 -> "d")).addOne(3 -> "c")
    assertEquals(m5(3), "c")

    val m6 = (m1 ++ List(4 -> "d")).addOne(3 -> "c")
    assertEquals(m6(3), "c")

    // deprecated
    val m7 = (m1 + (4 -> "d")).addOne(3 -> "c")
    assertEquals(m7(3), "c")

    // deprecated
    val m8 = m1.+(4 -> "d", 5 -> "e").addOne(3 -> "c")
    assertEquals(m8(3), "c")

    // deprecated
    val m9 = (m1.updated(4, "d")).addOne(3 -> "c")
    assertEquals(m9(3), "c")
  }

  import CollisionProofHashMapTest._

  @Test
  def treeifiesBucketFilledInDescendingHashOrder(): Unit = {
    // `hashCode` values for which the improved hash is `i << 16`: all keys share bucket 0 of any table up to 2^16 slots
    def key(i: Int) = { val h = i << 16; new Key(i, h ^ (h >>> 16)) }
    val n = 30000
    for (ascending <- List(true, false)) {
      val m = mutable.CollisionProofHashMap.empty[Key, Int]
      for (i <- if (ascending) 1 to n else n to 1 by -1) m(key(i)) = i
      assertEquals(n, m.size)
      assertEquals(s"ascending=$ascending", 0, longestListBucket(m))
      // a lookup in a balanced tree of 30000 nodes compares against at most 2 * log2(30001) < 30 keys
      for (i <- List(1, 2, n / 2, n - 1, n)) {
        Key.comparisons = 0
        assertEquals(Some(i), m.get(key(i)))
        assertTrue(s"ascending=$ascending: ${Key.comparisons} comparisons to find key $i", Key.comparisons <= 30)
      }
      assertEquals(None, m.get(key(n + 1)))
      for (i <- 1 to n) assertEquals(Some(i), m.remove(key(i)))
      assertTrue(m.isEmpty)
    }
  }

  @Test
  def treeifiesBucketRegardlessOfInsertionOrder(): Unit = {
    val rnd = new scala.util.Random(42)
    for (_ <- 0 until 100) {
      // 64 hash codes that all land in bucket 0 of a table with 1024 slots, a few keys for each
      val keys = rnd.shuffle(List.tabulate(200)(i => new Key(i, (i % 64) << 10)))
      val m = new mutable.CollisionProofHashMap[Key, Int](1024, 0.75)
      keys.foreach(k => m(k) = k.id)
      assertEquals(0, longestListBucket(m))
      keys.foreach(k => assertEquals(Some(k.id), m.get(k)))
    }
  }
}

object CollisionProofHashMapTest {
  /** A key with a chosen hash code, ordered by `id`. */
  final class Key(val id: Int, val hash: Int) {
    override def hashCode: Int = hash
    override def equals(o: Any): Boolean = o match { case k: Key => k.id == id; case _ => false }
    override def toString = s"Key($id, $hash)"
  }
  object Key {
    var comparisons = 0
    implicit val ordering: Ordering[Key] = (x, y) => { comparisons += 1; Integer.compare(x.id, y.id) }
  }

  /** The length of the longest bucket that is a linked list rather than a tree, when it is longer than the
   *  8 nodes at which a bucket is supposed to become a tree, otherwise 0.
   */
  def longestListBucket(m: CollisionProofHashMap[_, _]): Int = {
    val tableField = m.getClass.getDeclaredFields.find(_.getName.endsWith("table")).get
    tableField.setAccessible(true)
    def length(node: AnyRef): Int =
      if (node == null || !node.getClass.getName.endsWith("LLNode")) 0
      else {
        val next = node.getClass.getDeclaredField("next")
        next.setAccessible(true)
        var len = 0
        var n = node
        while (n != null) { len += 1; n = next.get(n) }
        len
      }
    val longest = tableField.get(m).asInstanceOf[Array[AnyRef]].iterator.map(length).max
    if (longest > 8) longest else 0
  }
}
