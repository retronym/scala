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

package scala.collection.mutable

import java.util.ConcurrentModificationException

import org.junit.Test

import scala.tools.testkit.AssertUtil.assertThrows

class MutationTrackerTest {
  @Test
  def checker(): Unit = {
    val tracker = MutationTracker()
    val checker = tracker.checker
    checker.checkMutations("") // does not throw
    tracker.addMutation()
    assertThrows[ConcurrentModificationException](checker.checkMutations("test"), _ == "test")
  }

  @Test
  def checkedIterator(): Unit = {
    val tracker = MutationTracker()
    val it1 = tracker.checkedIterator(List(1, 2, 3).iterator)
    it1.toList // does not throw
    val it2 = tracker.checkedIterator(List(1, 2, 3).iterator)
    tracker.addMutation()
    assertThrows[ConcurrentModificationException](it2.toList, _ contains "iteration")
  }
}
