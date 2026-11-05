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

import java.util.concurrent.atomic.LongAdder
import java.util.concurrent.locks.ReentrantLock

/** The lock under which every completion of a compiler used from several threads runs (`-Yparallel-typer`):
 *  one reentrant lock, the "serialised" mode of the parallel typer. It is the facade over the concurrency
 *  primitive, so that a claim per completion root can replace it.
 *
 *  It counts what it costs: the time threads waited for it, and the time it was held from the outermost
 *  acquisition. The holder's start time needs no synchronisation: only the holder writes it.
 */
final class CompletionLock {
  private[this] val lock = new ReentrantLock
  private[this] var holdStart = 0L

  val waitNanos = new LongAdder
  val waits = new LongAdder
  val holdNanos = new LongAdder
  val acquisitions = new LongAdder

  def isHeldByCurrentThread: Boolean = lock.isHeldByCurrentThread
  def owner: String = lock.toString

  def acquire(): Unit = {
    if (!lock.tryLock()) {
      val t0 = System.nanoTime()
      lock.lock()
      waitNanos.add(System.nanoTime() - t0)
      waits.increment()
      PerThreadTimes.addWait(System.nanoTime() - t0)
    }
    if (lock.getHoldCount == 1) {
      holdStart = System.nanoTime()
      acquisitions.increment()
    }
  }

  def release(): Unit = {
    if (lock.getHoldCount == 1) {
      val held = System.nanoTime() - holdStart
      holdNanos.add(held)
      PerThreadTimes.addHold(held)
    }
    lock.unlock()
  }

  @inline def apply[T](body: => T): T = {
    acquire()
    try body
    finally release()
  }

  /** Wait until no other thread holds the lock: what another thread was completing is then done. */
  def await(): Unit = { acquire(); release() }

  def reset(): Unit = { waitNanos.reset(); waits.reset(); holdNanos.reset(); acquisitions.reset() }

  def summary: String =
    f"completion lock: held ${holdNanos.sum / 1e6}%.1f ms in ${acquisitions.sum} outermost acquisitions, waited ${waitNanos.sum / 1e6}%.1f ms in ${waits.sum} waits"
}

/** Wait and hold times of the completion lock on the current thread, for a timeline of a parallel phase. */
object PerThreadTimes {
  final class Times { var waitNanos = 0L; var holdNanos = 0L }
  private[this] val times = ThreadLocal.withInitial[Times](() => new Times)
  def addWait(n: Long): Unit = times.get.waitNanos += n
  def addHold(n: Long): Unit = times.get.holdNanos += n
  def current: Times = times.get
}
