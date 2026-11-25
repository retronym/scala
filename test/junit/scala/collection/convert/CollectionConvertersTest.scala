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

import org.junit.Assert.{assertEquals, assertNull, assertTrue}
import org.junit.Test

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

  @Test def `Buffer asJava supports the mutators that AbstractList derives from add and remove`(): Unit = {
    import scala.collection.mutable.ArrayBuffer
    def check(expected: List[Int])(f: java.util.List[Integer] => Unit): Unit = {
      val b = ArrayBuffer[Integer](1, 2, 3, 4)
      f(b.asJava)
      assertEquals(expected, b.toList.map(_.intValue))
    }
    check(List(0, 1, 2, 3, 4))(_.add(0, 0))
    check(List(1, 7, 8, 2, 3, 4))(_.addAll(1, java.util.Arrays.asList(7, 8)))
    check(List(1, 2, 9, 3, 4))(_.listIterator(2).add(9))
    check(List(1, 3, 4))(l => assertTrue(l.remove(Integer.valueOf(2): Object)))
    check(List(1, 3))(l => assertTrue(l.removeIf(_ % 2 == 0)))
    check(List(2, 4))(l => assertTrue(l.removeAll(java.util.Arrays.asList(1, 3))))
    check(List(1, 3))(l => assertTrue(l.retainAll(java.util.Arrays.asList(1, 3))))
    check(List(1, 2, 3, 4))(l => assertTrue(!l.remove(Integer.valueOf(5): Object)))
  }
}
