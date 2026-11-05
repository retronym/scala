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
import io.github.retronym.sharing.{Claimant, CoarseLock, CycleHandler, PerKeyClaims, What}

/** The lock under which every completion of a compiler used from several threads runs (`-Yparallel-typer`):
 *  one reentrant lock, the "serialised" mode of the parallel typer. It is the facade over the concurrency
 *  primitive, so that a claim per completion root can replace it.
 *
 *  It counts what it costs: the time threads waited for it, and the time it was held from the outermost
 *  acquisition. The holder's start time needs no synchronisation: only the holder writes it.
 */
final class CompletionLock(perKey: Boolean = false, cycle: AnyRef => Nothing = s => throw new IllegalStateException(s"wait-for cycle on $s")) {
  // scala-sharing's coarse registry: one reentrant lock that keeps what its holder is doing, for the reports of the
  // threads that wait (`io.github.retronym.sharing.CoarseLock`).
  private[this] val lock = new CoarseLock((_, _, _, _) => ())

  // With `perKey` (`-Dscalac.parallelTyper.claims=perKey`), a claim per completion root (`PerKeyClaims`): the coarse
  // root (`CompletionLock.CoarseRoot`, every acquisition without a key) and a source symbol's. A claimant per thread,
  // which cannot abort (a batch completion cannot be retried); a wait-for cycle is reported to `cycle`.
  private[this] val claimants: ThreadLocal[Claimant] =
    ThreadLocal.withInitial(() => new Claimant(Thread.currentThread) {
      override def canAbort = false
      override def recordWait(nanos: Long, holderWhat: String, holderName: String): Unit = {
        waitNanos.add(nanos); waits.increment(); PerThreadTimes.addWait(nanos)
      }
    })
  private[this] val claims: PerKeyClaims =
    if (perKey) new PerKeyClaims(() => claimants.get, new CycleHandler { def waitCycle(s: AnyRef): Nothing = cycle(s) }) else null
  def isPerKey: Boolean = claims ne null

  /** `body` holding the claim on `key` (in the coarse mode, the lock), to do `what`; a wait-for cycle on the way is
   *  reported against `subject`. */
  def claim[T](key: AnyRef, what: What, subject: AnyRef)(body: => T): T =
    if (claims eq null) { acquire(what); try body finally release() }
    else {
      claims.acquire(key, what, () => cycle(subject))
      if (claims.ownedHere(key)) acquisitions.increment()
      try body finally claims.release(key)
    }

  /** Claim `key` (per-key mode only); a wait that would close a wait-for cycle calls `onCycle`. */
  def acquireKey(key: AnyRef, what: What, onCycle: () => Nothing): Unit = {
    claims.acquire(key, what, onCycle)
    acquisitions.increment()
  }
  def releaseKey(key: AnyRef): Unit = claims.release(key)

  /** Wait until no other thread holds the claim on `key` (the coarse mode: the lock). A wait that would close a
   *  wait-for cycle calls `onCycle` (which throws). */
  def awaitKey(key: AnyRef, subject: AnyRef, onCycle: () => Nothing = null): Unit =
    if (claims eq null) await()
    else {
      claims.acquire(key, CompletionLock.waiting, if (onCycle ne null) onCycle else () => cycle(subject))
      claims.release(key)
    }
  private[this] var holdStart = 0L

  val waitNanos = new LongAdder
  val waits = new LongAdder
  val holdNanos = new LongAdder
  val acquisitions = new LongAdder

  def isHeldByCurrentThread: Boolean = lock.isHeldByCurrentThread
  def isLocked: Boolean = lock.isLocked
  def owner: String = lock.toString

  def acquire(): Unit = acquire(CompletionLock.completing)

  /** Acquire the lock (the coarse root's claim) to do `what` (a site label for the reports of waiting threads). */
  def acquire(what: What): Unit =
    if (claims ne null) { claims.acquire(CompletionLock.CoarseRoot, what, () => cycle(CompletionLock.CoarseRoot)) }
    else acquireLock(what)

  private def acquireLock(what: What): Unit = {
    val xray = ParallelTyperTimeline.current; if (xray ne null) { xray.acquire(this, lock, what); return } // xray
    val contended = lock.isLocked && !lock.isHeldByCurrentThread
    val t0 = if (contended) System.nanoTime() else 0L
    lock.lockFor(what)
    if (contended) {
      val waited = System.nanoTime() - t0
      waitNanos.add(waited)
      waits.increment()
      PerThreadTimes.addWait(waited)
    }
    if (lock.getHoldCount == 1) {
      holdStart = System.nanoTime()
      acquisitions.increment()
      if (sites ne null) recordSite()
    }
  }

  /** `acquire`, naming what is completed for the x-ray (`ParallelTyperTimeline`). */
  def acquire(what: AnyRef): Unit = { val xray = ParallelTyperTimeline.current; if (xray ne null) xray.acquire(this, lock, what) else acquire() } // xray

  /** `-Dscalac.parallelTyper.lockSites`: count the outermost acquisitions by the first caller outside the lock's
   *  plumbing (slow: a stack walk per acquisition). */
  private[this] val sites: java.util.concurrent.ConcurrentHashMap[String, LongAdder] =
    if (System.getProperty("scalac.parallelTyper.lockSites") != null) new java.util.concurrent.ConcurrentHashMap else null
  private def recordSite(): Unit = {
    val frames = new Throwable().getStackTrace
    val plumbing = Set("acquire", "apply", "underCompletionLock", "completeInfoUnderLock", "await", "awaitCompletionElsewhere")
    val f = frames.find(f => !plumbing(f.getMethodName) && !f.getClassName.endsWith("CompletionLock")).getOrElse(frames.last)
    sites.computeIfAbsent(s"${f.getClassName}.${f.getMethodName}", _ => new LongAdder).increment()
  }
  def siteSummary: String =
    if (sites eq null) ""
    else {
      import scala.jdk.CollectionConverters._
      sites.asScala.toList.map { case (k, v) => (k, v.sum) }.sortBy(-_._2).take(25).map { case (k, v) => f"$v%10d $k" }.mkString("\n", "\n", "")
    }

  def release(): Unit =
    if (claims ne null) claims.release(CompletionLock.CoarseRoot)
    else releaseLock()

  private def releaseLock(): Unit = {
    val xray = ParallelTyperTimeline.current; if (xray ne null) { xray.release(this, lock); return } // xray
    if (lock.getHoldCount == 1) {
      val held = System.nanoTime() - holdStart
      holdNanos.add(held)
      PerThreadTimes.addHold(held)
    }
    lock.unlockFor()
  }

  @inline def apply[T](body: => T): T = {
    acquire()
    try body
    finally release()
  }

  /** Wait until no other thread holds the lock: what another thread was completing is then done. */
  def await(): Unit = { acquire(); release() }

  def isHeldHere: Boolean = if (claims ne null) claims.ownedHere(CompletionLock.CoarseRoot) else lock.isHeldByCurrentThread

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

object CompletionLock {
  /** The label of a completion's acquisition. */
  val completing: What = new What("completing a symbol", null)
  /** The label of a wait for another thread's completion. */
  val waiting: What = new What("waiting for a completion", null)
  /** The claim of everything that is not a source symbol's completion: the classpath (loaders, unpickler, classfile
   *  parser, which share state across files), the type caches still under the lock, macro expansion. Coarse. */
  object CoarseRoot
  val coarse: What = new What("the classpath and shared caches", null, coarse = true)
  /** Thrown by a wait for another thread's completion that would close a wait-for cycle, to the waiter, which then
   *  reads a provisional info if the completion has published one (as a serial nesting would), or reports the cycle. */
  object WaitCycle extends scala.util.control.ControlThrowable
}
