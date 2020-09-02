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
 * Utilities to that tracks mutations a client is supposed to
 * to have its iterator check that the client hasn't mutated
 * itself since the iterator's creation either by wrapping the
 * iterator using [[checkedIterator]] or by manually using a
 * [[checker]].
 */
private object MutationTracker {
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
    def checkMutations(mutationCount: Int, expectedCount: Int, message: String): Unit = {
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
    @inline def checkMutationsForIteration(mutationCount: Int, expectedCount: Int): Unit =
      checkMutations(mutationCount, expectedCount, "mutation occurred during iteration")

  final class CheckedIterator[A](underlying: Iterator[A], mutationCount: () => Int) extends AbstractIterator[A] {
    private[this] val expectedCount = mutationCount()
    def hasNext: Boolean = {
      MutationTracker.checkMutationsForIteration(mutationCount(), expectedCount)
      underlying.hasNext
    }
    def next(): A = underlying.next()
  }
}
