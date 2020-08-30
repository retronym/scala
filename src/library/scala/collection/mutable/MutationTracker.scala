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

package scala
package collection
package mutable

import java.util.ConcurrentModificationException

/**
 * A utility that tracks mutations; a client is meant to
 * call [[addMutation()]] each time it mutates itself, and
 * to have its iterator check that the client hasn't mutated
 * itself since the iterator's creation either by wrapping the
 * iterator using [[checkedIterator]] or by manually using a
 * [[checker]].
 */
private final class MutationTracker private {
  private[this] var mutationCount: Int = 0

  def addMutation(): Unit = mutationCount += 1

  def checker: Checker = new Checker

  def checkedIterator[A](it: Iterator[A]): Iterator[A] = new CheckedIterator(it)

  /**
   * A utility that can be used to check that its creator
   * has not mutated itself since this checker's creation.
   */
  final class Checker private[MutationTracker] {
    private[this] val expectedCount = mutationCount

    /**
     * Checks whether or not this checker's creator has mutated
     * itself since this checker's creation, throwing an exception,
     * if it has.
     *
     * @param message the exception message in case of mutations
     * @throws ConcurrentModificationException if this checker's creator
     *                                         has mutated itself
     */
    @throws[ConcurrentModificationException]
    def checkMutations(message: String): Unit = {
      if (mutationCount != expectedCount) throw new ConcurrentModificationException(message)
    }

    /**
     * Checks whether or not this checker's creator has mutated
     * itself since this checker's creation, throwing an exception,
     * if it has. This method produces an exception message saying
     * that it was called by an iterator whose backing collection
     * was mutated after the iterator's creation.
     *
     * @throws ConcurrentModificationException if the collection has
     *                                         been mutated
     */
    @throws[ConcurrentModificationException]
    @inline def checkMutationsForIteration(): Unit =
      checkMutations("mutation occurred during iteration")
  }

  private final class CheckedIterator[A](underlying: Iterator[A]) extends AbstractIterator[A] {
    private[this] val checker = new Checker

    def hasNext: Boolean = {
      checker.checkMutationsForIteration()
      underlying.hasNext
    }
    def next(): A = underlying.next()
  }
}

private object MutationTracker {
  def apply(): MutationTracker = new MutationTracker
}
