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

import java.{util => ju}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.util.Random

class SeqWrapperTest {
  // a Seq without fast indexed access that counts the calls of `apply`
  final class CountingSeq[A](xs: List[A]) extends scala.collection.immutable.AbstractSeq[A] {
    var applies = 0
    def apply(i: Int): A = { applies += 1; xs(i) }
    def length: Int = xs.length
    def iterator: Iterator[A] = xs.iterator
  }

  final class CountingBuffer[A] extends ListBuffer[A] {
    var indexed = 0
    override def apply(i: Int): A = { indexed += 1; super.apply(i) }
    override def update(i: Int, a: A): Unit = { indexed += 1; super.update(i, a) }
  }

  @Test def `asJava of a linear Seq iterates without indexed access`(): Unit = {
    val n = 1000
    val xs = List.tabulate(n)(Integer.valueOf)
    val counts = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    def check(what: String)(f: ju.List[Integer] => Any): Unit = {
      val s = new CountingSeq(xs)
      f(s.asJava)
      counts(what) = s.applies
    }
    check("equals")(l => assertTrue(l.equals(new ju.ArrayList(xs.asJava))))
    check("indexOf")(l => assertEquals(n - 1, l.indexOf(n - 1)))
    check("indexOf absent")(l => assertEquals(-1, l.indexOf(n)))
    check("lastIndexOf")(l => assertEquals(0, l.lastIndexOf(0)))
    check("subList")(l => assertEquals(xs.slice(10, n - 10), l.subList(10, n - 10).asScala.toList))
    check("listIterator")(l => assertEquals(xs.drop(5), l.listIterator(5).asScala.toList))

    val b = new CountingBuffer[Integer]
    b ++= Random.shuffle(xs)
    b.asJava.sort(null)
    assertEquals(xs, b.toList)
    counts("sort") = b.indexed
    assertEquals(counts.map { case (k, _) => (k, 0) }, counts)
  }

  @Test def `list iterator of a linear Seq behaves as ArrayList's`(): Unit = {
    val rnd = new Random(42)
    for (_ <- 1 to 200) {
      val init = List.fill(rnd.nextInt(8))(Integer.valueOf(rnd.nextInt(100)))
      val expected = new ju.ArrayList[Integer](init.asJava)
      val buf = ListBuffer.from(init)
      val start = rnd.nextInt(init.length + 1)
      val ei = expected.listIterator(start)
      val ai = buf.asJava.listIterator(start)
      for (_ <- 1 to 20) {
        def same[T](op: ju.ListIterator[Integer] => T): Unit = {
          def run(it: ju.ListIterator[Integer]) = try Right(op(it)) catch { case e: RuntimeException => Left(e.getClass) }
          assertEquals(run(ei), run(ai))
        }
        // no `add`: the wrapper of a buffer does not support inserting at an index
        rnd.nextInt(6) match {
          case 0 => same(_.next())
          case 1 => same(_.previous())
          case 2 => same(_.remove())
          case 3 => val e = Integer.valueOf(rnd.nextInt(100)); same(_.set(e))
          case 4 => same(it => (it.hasNext, it.hasPrevious))
          case 5 => same(it => (it.nextIndex, it.previousIndex))
        }
        assertEquals(expected, buf.asJava)
      }
    }
    // the immutable wrapper is read only
    val l = List(1, 2, 3).asJava.listIterator()
    l.next()
    assertTrue(scala.util.Try(l.set(5)).isFailure)
    assertTrue(scala.util.Try(l.remove()).isFailure)
    assertTrue(scala.util.Try(l.add(5)).isFailure)
  }
}
