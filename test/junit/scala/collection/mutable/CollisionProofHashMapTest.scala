package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.util.Random

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
  def nanIsOneKey(): Unit = {
    val m = mutable.CollisionProofHashMap.empty[Double, Int]
    m(Double.NaN) = 1
    m(Double.NaN) = 2
    assertEquals(1, m.size)
    assertEquals(Some(2), m.get(Double.NaN))
    assertEquals(Some(2), m.remove(Double.NaN))
    assertEquals(0, m.size)
    m(Double.NaN) = 3
    m -= Double.NaN
    assertTrue(m.isEmpty)
  }

  @Test
  def keysAreIdentifiedByHashCodeAndOrdering(): Unit = {
    // by design, unlike mutable.HashMap: no universal equality, the Ordering decides
    val z = mutable.CollisionProofHashMap.empty[Double, Int]
    z(0.0) = 1
    z(-0.0) = 2
    assertEquals(2, z.size)
    assertEquals(Some(1), z.get(0.0))
    assertEquals(Some(2), z.remove(-0.0))
    assertEquals(Some(1), z.get(0.0))

    implicit val byValue: Ordering[Any] = Ordering.by((x: Any) => x.asInstanceOf[Number].doubleValue)
    val a = mutable.CollisionProofHashMap.empty[Any, Int]
    a(1) = 1
    a(1L) = 2  // same hashCode, equal under the ordering
    a(1.0) = 3 // equal under the ordering, but another hashCode
    assertEquals(2, a.size)
    assertEquals(Some(2), a.get(1))
    assertEquals(Some(2), a.get(1L))
    assertEquals(Some(3), a.get(1.0))
    assertEquals(Some(2), a.remove(1))
    assertEquals(None, a.remove(1L))
    assertEquals(Some(3), a.remove(1.0))
  }

  @Test
  def nullKeyInListBucket(): Unit = {
    val m = mutable.CollisionProofHashMap.empty[String, Int]
    m("") = 1 // shares hash code 0 with null
    m(null) = 2
    assertEquals(2, m.size)
    m(null) = 3
    assertEquals(2, m.size)
    assertEquals(Some(1), m.get(""))
    assertEquals(Some(3), m.get(null))
    assertEquals(Some(3), m.remove(null))
    assertEquals(None, m.get(null))
    assertEquals(Some(1), m.remove(""))
  }

  /** Runs the same operations on keys that sit in a short list bucket and on keys that sit in a treeified bucket.
   *  `Folded` keys are equal under the ordering when they differ only in case, which `equals` does not agree with.
   */
  @Test
  def listAndTreeBucketsAgreeOnKeyIdentity(): Unit = {
    for (treeified <- List(false, true)) {
      val m = new mutable.CollisionProofHashMap[Folded, Int](64, 0.75)
      // bucket 0 holds hash codes 0 and 64; with `treeified` enough other keys to make it a tree
      if (treeified) for (i <- 0 until 40) m(new Folded("filler" + i, (i % 2) * 64)) = -1
      def msg = s"treeified=$treeified"
      m(new Folded("a", 0)) = 1
      // same hash code, equal under the ordering: the same key
      m(new Folded("A", 0)) = 2
      assertEquals(msg, Some(2), m.get(new Folded("a", 0)))
      // another hash code in the same bucket: another key, although the ordering says equal
      m(new Folded("A", 64)) = 3
      assertEquals(msg, None, m.get(new Folded("a", 128)))
      assertEquals(msg, Some(2), m.get(new Folded("A", 0)))
      assertEquals(msg, Some(3), m.get(new Folded("a", 64)))
      assertEquals(msg, if (treeified) 42 else 2, m.size)
      assertEquals(msg, Some(2), m.remove(new Folded("a", 0)))
      assertEquals(msg, None, m.remove(new Folded("A", 0)))
      assertEquals(msg, Some(3), m.remove(new Folded("a", 64)))
      assertEquals(msg, if (treeified) 40 else 0, m.size)
    }
  }

  @Test
  def differentialAgainstHashMap(): Unit = {
    for (seed <- 0 until 200) {
      val rnd = new Random(seed)
      // few distinct hash codes force long (treeified) buckets, many leave short lists
      val hashes = Array(1, 2, 3, 17, 1000)(seed % 5)
      val keyspace = Array(20, 200, 2000)(seed % 3)
      def key() = { val id = rnd.nextInt(keyspace); new Key(id, (id % hashes) * (if (seed % 7 == 0) 0x10001 else 1)) }
      val c = new mutable.CollisionProofHashMap[Key, Int](rnd.nextInt(100), Array(0.75, 0.5, 2.0, 0.9)(rnd.nextInt(4)))
      val r = new mutable.HashMap[Key, Int]()
      for (step <- 0 until 300) {
        val k = key()
        val v = rnd.nextInt(1000)
        val op = rnd.nextInt(10)
        def msg = s"seed=$seed step=$step op=$op key=$k"
        op match {
          case 0 | 1 | 2 | 3 => c(k) = v; r(k) = v
          case 4 | 5 | 6 => assertEquals(msg, r.remove(k), c.remove(k))
          case 7 => assertEquals(msg, r.getOrElseUpdate(k, v), c.getOrElseUpdate(k, v))
          case 8 => val xs = List.fill(rnd.nextInt(50))((key(), rnd.nextInt(9))); c ++= xs; r ++= xs
          case 9 => val xs = List.fill(rnd.nextInt(50))(key()); c --= xs; r --= xs
        }
        assertEquals(msg, r.get(k), c.get(k))
        assertEquals(msg, r.size, c.size)
        if (step % 10 == 0) {
          assertEquals(msg, r, c.iterator.toMap)
          r.foreach { case (k, v) => assertEquals(msg, Some(v), c.get(k)) }
        }
      }
      assertEquals(s"seed=$seed", r, c)
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
    implicit val ordering: Ordering[Key] = Ordering.by(_.id)
  }

  /** A key with a chosen hash code that is ordered ignoring case, but `equals` only the same string. */
  final class Folded(val s: String, val hash: Int) {
    override def hashCode: Int = hash
    override def equals(o: Any): Boolean = o match { case f: Folded => f.s == s; case _ => false }
    override def toString = s"Folded($s, $hash)"
  }
  object Folded {
    implicit val ordering: Ordering[Folded] = Ordering.by(_.s.toLowerCase)
  }
}
