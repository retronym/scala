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

package scala.collection.immutable

import org.junit.Assert.{assertEquals, assertSame}
import org.junit.{Ignore, Test}

import scala.tools.testkit.AllocationTest
import scala.tools.testkit.AssertUtil.assertThrows

class BitSetTest extends AllocationTest {
  val n = 10000
  val ints: List[Int] = List.range(0, n)
  val all: BitSet = BitSet.fromSpecific(ints)
  val oddInts: List[Int] = ints.filter(_ % 2 == 1)
  val odd: BitSet = BitSet.fromSpecific(oddInts)

  // one copy of the words (157 longs at n = 10,000), not one per element
  @Test def `concat of a collection that is not a bitset copies the words once`(): Unit =
    onlyAllocates(8 * 1024)(BitSet.empty ++ ints)

  // removedAll still folds excl: overriding it in immutable.BitSet adds a method signature that 2.13.18 lacks
  @Ignore("needs an override of removedAll in immutable.BitSet, which is not forward binary compatible")
  @Test def `removedAll of a bitset is word-wise`(): Unit =
    onlyAllocates(4 * 1024)(all -- odd)

  @Ignore("needs an override of removedAll in immutable.BitSet, which is not forward binary compatible")
  @Test def `removedAll of a collection that is not a bitset copies the words once`(): Unit =
    onlyAllocates(4 * 1024)(all -- oddInts)

  @Test def `bulk concat and removedAll agree with incl and excl`(): Unit = {
    val r = new scala.util.Random(7)
    for (_ <- 1 to 200) {
      val base = BitSet.fromSpecific(List.fill(r.nextInt(20))(r.nextInt(300)))
      val xs = List.fill(r.nextInt(20))(r.nextInt(400))
      assertEquals(xs.foldLeft(base)(_ incl _), base ++ xs)
      assertEquals(xs.foldLeft(base)(_ excl _), base -- xs)
      val bs = BitSet.fromSpecific(xs)
      assertEquals(xs.foldLeft(base)(_ excl _), base -- bs)
      assertEquals((base ++ xs).toBitMask.toList, xs.foldLeft(base)(_ incl _).toBitMask.toList)
      assertEquals((base -- xs).toBitMask.toList, xs.foldLeft(base)(_ excl _).toBitMask.toList)
    }
    assertSame(all, all ++ List(1, 2, 3))
    assertSame(odd, odd -- List(0, 2, 4, n * 2))
    assertThrows[IllegalArgumentException](all ++ List(1, -1))
    assertThrows[IllegalArgumentException](all -- List(1, -1))
  }
}
