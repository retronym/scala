/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util

import java.util.concurrent.atomic.AtomicInteger

/** A source of ids: plain in a universe used from one thread at a time, atomic in a concurrent one. */
abstract class IdCounter {
  /** The next id, starting at 1. */
  def next(): Int
  /** The last id handed out, 0 if none. */
  def current: Int
}

object IdCounter {
  def apply(concurrent: Boolean): IdCounter = if (concurrent) new Atomic else new Plain

  private final class Plain extends IdCounter {
    private[this] var last = 0
    def next(): Int = { last += 1; last }
    def current: Int = last
  }

  private final class Atomic extends IdCounter {
    private[this] val last = new AtomicInteger(0)
    def next(): Int = last.incrementAndGet()
    def current: Int = last.get
  }
}
