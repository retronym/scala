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

package scala.collection.convert

import java.util.{
  Dictionary,
  HashMap    => JMap,
  Hashtable  => JTable,
  Properties => JProperties,
}
import java.util.concurrent.{
  ConcurrentHashMap => JCMap,
}

import org.junit.Assert.{assertEquals, assertFalse, assertNull, assertTrue}
import org.junit.Test

import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters._
import scala.tools.testkit.AssertUtil.assertThrows

class CollectionConvertersTest {
  // Binding to null is represented as Some(null).
  // This is consistent with get, which is the basis for getOrElse, lift, unapply
  //
  @Test def `t11894 Map wrapper respects put contract`(): Unit = {
    val sut = new JMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JMapWrapper[_, _]])
    assertThrows[NoSuchElementException](sut("one"))
    assertEquals(None, sut.unapply("one"))
    assertEquals(None, sut.put("one", null))
    assertNull(sut("one"))
    assertEquals(Some(null), sut.unapply("one"))
    assertEquals(Some(null), sut.lift("one"))
    assertEquals(Some(null), sut.put("one", "eins"))
    assertEquals(Some("eins"), sut.put("one", "uno"))
    assertEquals(Some("uno"), sut.remove("one"))
    assertEquals(None, sut.remove("one"))
    assertEquals(None, sut.put("one", null))
    assertEquals(Some(null), sut.remove("one"))
    assertEquals(None, sut.remove("one"))
  }
  @Test def `t11894 Map wrapper iterator witnesses null values`(): Unit = {
    val sut = new JMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JMapWrapper[_, _]])
    assertEquals(None, sut.put("one", null))
    assertEquals(None, sut.put("deuce", "zwey"))
    assertTrue(sut.iterator.find { case (_, v) => v == null }.nonEmpty)
  }
  @Test def `t11894 Dictionary wrapper disallows nulls`(): Unit = {
    val tbl = new JTable[String, String]
    val sut = tbl.asInstanceOf[Dictionary[String, String]].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JDictionaryWrapper[_, _]])
    assertThrows[NullPointerException](sut.put("any", null))
  }
  @Test def `t11894 Properties wrapper enforces Strings`(): Unit = {
    val ps  = new JProperties()
    val sut = ps.asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JPropertiesWrapper])
    assertThrows[NullPointerException](sut.put("any", null))
    assertEquals(None, sut.put("one", "eins"))
    assertEquals("eins", sut("one"))
    ps.put("one", new Object())
    assertThrows[ClassCastException](sut.put("one", "uno"))
  }
  @Test def `t11894 JDK concurrent Map impl disallows nulls`(): Unit = {
    val sut = new JCMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JConcurrentMapWrapper[_, _]])
    assertThrows[NullPointerException](sut.put("any", null))
  }

  // A null key is not key 0: java.util allows a null probe to answer false or null, or to throw NullPointerException.
  @Test def `asJava of an Int or Long keyed collection does not read a null key as 0`(): Unit = {
    def absent(answer: => Any, none: Any): Unit =
      try assertEquals(none, answer) catch { case _: NullPointerException => }
    def unchanged[C](coll: C)(op: C => Any): Unit = {
      val before = coll.toString
      try op(coll) catch { case _: NullPointerException => }
      assertEquals(before, coll.toString)
    }
    absent(mutable.BitSet(0, 5).asJava.contains(null), false)
    absent(immutable.BitSet(0, 5).asJava.contains(null), false)
    absent(immutable.TreeSet[Int](0, 5).asJava.contains(null), false)
    absent(mutable.TreeSet[Int](0, 5).asJava.contains(null), false)
    absent(mutable.LongMap(0L -> "a").asJava.get(null), null)
    absent(mutable.LongMap(0L -> "a").asJava.containsKey(null), false)
    absent(immutable.IntMap(0 -> "a").asJava.get(null), null)
    absent(immutable.TreeMap[Int, String](0 -> "a").asJava.get(null), null)
    absent(immutable.TreeMap[Long, String](0L -> "a").asJava.get(null), null)
    unchanged(mutable.BitSet(0, 5))(_.asJava.remove(null))
    unchanged(mutable.BitSet(5))(_.asJava.asInstanceOf[java.util.Set[Any]].add(null))
    unchanged(mutable.LongMap(0L -> "a"))(_.asJava.remove(null))
    unchanged(mutable.LongMap(0L -> "a"))(_.asJava.asInstanceOf[java.util.Map[Any, String]].put(null, "b"))
    // collections that can hold null must keep answering for it
    assertTrue(Set[String]("a", null).asJava.contains(null))
    assertFalse(Set[String]("a").asJava.contains(null))
    assertEquals(1, mutable.HashMap[String, Int]((null: String) -> 1).asJava.get(null))
  }
}
