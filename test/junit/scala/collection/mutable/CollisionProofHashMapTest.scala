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
  def putReturnsPreviousValue(): Unit = {
    val m = mutable.CollisionProofHashMap.empty[String, Int]
    assertEquals(None, m.put("a", 1))
    assertEquals(Some(1), m.put("a", 2))
    assertEquals(None, m.put("b", 1))
    assertEquals(Some(2), m.get("a"))

    // more than `treeifyThreshold` keys with the same hash code turn the bucket into a red-black tree
    val t = mutable.CollisionProofHashMap.empty[Key, Int]
    for (i <- 0 until 20) assertEquals(None, t.put(new Key(i, 0), i))
    for (i <- 0 until 20) assertEquals(Some(i), t.put(new Key(i, 0), -i))
    for (i <- 0 until 20) assertEquals(Some(-i), t.get(new Key(i, 0)))
    assertEquals(None, t.put(new Key(20, 0), 20))
    assertEquals(21, t.size)
  }

  @Test
  def differentialAgainstHashMap(): Unit = {
    for (seed <- 0 until 200) {
      val rnd = new Random(seed)
      // few distinct hash codes force long (treeified) buckets, many leave short lists
      val hashes = Array(1, 2, 3, 17, 1000)(seed % 5)
      val keyspace = Array(20, 200, 2000)(seed % 3)
      def key() = { val id = rnd.nextInt(keyspace); new Key(id, (id % hashes) * (if (seed % 7 == 0) 0x10001 else 1)) }
      val c =
        if (seed % 2 == 0) new mutable.CollisionProofHashMap[Key, Int]()
        else new mutable.CollisionProofHashMap[Key, Int](rnd.nextInt(100), Array(0.75, 0.5, 2.0, 0.9)(rnd.nextInt(4)))
      val r = new mutable.HashMap[Key, Int]()
      for (step <- 0 until 300) {
        val k = key()
        val v = rnd.nextInt(1000)
        val op = rnd.nextInt(16)
        def msg = s"seed=$seed step=$step op=$op key=$k"
        op match {
          case 0 | 1 | 2 | 3 => assertEquals(msg, r.put(k, v), c.put(k, v))
          case 4 | 5 => c(k) = v; r(k) = v
          case 6 | 7 | 8 => assertEquals(msg, r.remove(k), c.remove(k))
          case 9 => c -= k; r -= k
          case 10 => assertEquals(msg, r.getOrElseUpdate(k, v), c.getOrElseUpdate(k, v))
          case 11 =>
            val f = (o: Option[Int]) => if (v % 2 == 0) None else Some(o.getOrElse(0) + v)
            assertEquals(msg, r.updateWith(k)(f), c.updateWith(k)(f))
          case 12 => c.filterInPlace((_, v) => v % 3 != 0); r.filterInPlace((_, v) => v % 3 != 0)
          case 13 => val xs = List.fill(rnd.nextInt(50))((key(), rnd.nextInt(9))); c ++= xs; r ++= xs
          case 14 => val xs = List.fill(rnd.nextInt(50))(key()); c --= xs; r --= xs
          case 15 => if (rnd.nextInt(20) == 0) { c.clear(); r.clear() }
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
}
