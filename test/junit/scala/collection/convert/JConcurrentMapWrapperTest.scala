package scala.collection.convert

import org.junit.Assert.assertEquals
import org.junit.Test

import java.{util => ju}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, ConcurrentSkipListMap}

import scala.collection.concurrent.ConcurrentMapTestHelper
import scala.jdk.CollectionConverters._

class JConcurrentMapWrapperTest {
  @Test
  def CHM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CHM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CSLM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }

  @Test
  def CSLM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }

  import JConcurrentMapWrapperTest.InterleavedMap

  // `put` and `remove` of the wrapper while another thread, played by the action given to `interleave`, updates the
  // map between a lookup and the update that follows it. The action only runs if the wrapper makes such a lookup;
  // if it does not, the operation is alone and returns the value from before.

  // The other thread removes the entry that the first attempt of `compute` has seen; the second attempt sees none.
  @Test
  def putAfterLosingToRemove(): Unit = {
    val m = new InterleavedMap
    m.put("a", 1)
    m.interleave(() => assertEquals(1, m.remove("a")))
    val res = m.asScala.put("a", 2)
    assertEquals(if (m.interleaved) None else Some(1), res)
    assertEquals(2, m.get("a"))
  }

  @Test
  def removeAfterLosingToRemove(): Unit = {
    val m = new InterleavedMap
    m.put("a", 1)
    m.interleave(() => assertEquals(1, m.remove("a")))
    val res = m.asScala.remove("a")
    assertEquals(if (m.interleaved) None else Some(1), res)
    assertEquals(null, m.get("a"))
  }

  // The same through the wrapper for a `java.util.Map`, which calls `compute`.
  @Test
  def putAndRemoveAfterLosingToRemoveAsPlainMap(): Unit = {
    val m = new InterleavedMap
    m.put("a", 1)
    m.interleave(() => assertEquals(1, m.remove("a")))
    assertEquals(None, (m: ju.Map[String, Integer]).asScala.put("a", 2))
    assert(m.interleaved)
    m.interleave(() => assertEquals(2, m.remove("a")))
    assertEquals(None, (m: ju.Map[String, Integer]).asScala.remove("a"))
    assert(m.interleaved)
  }

  // The other thread inserts the key after `compute` has found it absent. Null is never stored.
  @Test
  def putWhileInserted(): Unit = {
    val m = new InterleavedMap
    m.interleave(() => assertEquals(null, m.put("a", 1)))
    val res = m.asScala.put("a", 2)
    val ok =
      if (m.interleaved) res == None && m.get("a") == 1 || res == Some(1) && m.get("a") == 2
      else res == None && m.get("a") == 2
    assert(ok, s"put returned $res, the map has ${m.get("a")}")
  }

  @Test
  def removeWhileInserted(): Unit = {
    val m = new InterleavedMap
    m.interleave(() => assertEquals(null, m.put("a", 1)))
    val res = m.asScala.remove("a")
    val ok =
      if (m.interleaved) res == None && m.get("a") == 1 || res == Some(1) && m.get("a") == null
      else res == None && m.get("a") == null
    assert(ok, s"remove returned $res, the map has ${m.get("a")}")
  }

  @Test
  def putAndRemovePreviousValue(): Unit =
    for (m <- List[ConcurrentMap[String, Integer]](new ConcurrentHashMap, new ConcurrentSkipListMap, new InterleavedMap)) {
      val s = m.asScala
      assertEquals(None, s.put("a", 1))
      assertEquals(Some(1), s.put("a", 2))
      assertEquals(Some(2), s.remove("a"))
      assertEquals(None, s.remove("a"))
      assertEquals(0, m.size)
    }
}

object JConcurrentMapWrapperTest {
  /** A `ConcurrentMap` that keeps the default methods of the interface, `compute` among them, which applies its
   *  function again after losing a race. `interleave` schedules an action, standing for another thread, to run
   *  once: after the next lookup that the wrapper or `compute` makes and before its next update.
   */
  class InterleavedMap extends ju.AbstractMap[String, Integer] with ConcurrentMap[String, Integer] {
    private[this] val underlying = new ConcurrentHashMap[String, Integer]
    private[this] var pending: () => Unit = null
    private[this] var armed = false
    private[this] var ran = false
    def interleave(action: () => Unit): Unit = { pending = action; armed = false; ran = false }
    /** Has the action given to `interleave` run? */
    def interleaved: Boolean = ran
    private def lookedUp(): Unit = if (pending != null) armed = true
    private def beforeUpdate(): Unit = if (armed) { val p = pending; pending = null; armed = false; ran = true; p() }

    override def entrySet(): ju.Set[ju.Map.Entry[String, Integer]] = underlying.entrySet()
    override def get(k: AnyRef): Integer = try underlying.get(k) finally lookedUp()
    override def containsKey(k: AnyRef): Boolean = { beforeUpdate(); underlying.containsKey(k) }
    override def put(k: String, v: Integer): Integer = { beforeUpdate(); underlying.put(k, v) }
    override def remove(k: AnyRef): Integer = { beforeUpdate(); underlying.remove(k) }
    override def putIfAbsent(k: String, v: Integer): Integer = { beforeUpdate(); underlying.putIfAbsent(k, v) }
    override def remove(k: AnyRef, v: AnyRef): Boolean = { beforeUpdate(); underlying.remove(k, v) }
    override def replace(k: String, o: Integer, n: Integer): Boolean = { beforeUpdate(); underlying.replace(k, o, n) }
    override def replace(k: String, v: Integer): Integer = { beforeUpdate(); underlying.replace(k, v) }
  }
}
