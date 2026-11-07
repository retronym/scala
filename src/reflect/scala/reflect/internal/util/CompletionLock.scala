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
import scala.reflect.internal.sharing.{Claimant, CycleHandler, PerKeyClaims, What}

/** The claims under which every completion of a compiler used from several threads runs (`-Yparallel-typer`): a
 *  claim per completion root (scala-sharing's `PerKeyClaims`), the coarse root (`CompletionLock.CoarseRoot`, every
 *  acquisition without a key) or a source symbol's. It is the facade over the concurrency primitive.
 *
 *  It counts what it costs: the time threads waited for a claim, and the outermost acquisitions.
 */
final class CompletionLock(cycle: AnyRef => Nothing = s => throw new IllegalStateException(s"wait-for cycle on $s")) {
  // A claimant per thread, which cannot abort (a batch completion cannot be retried); a wait-for cycle is reported to
  // `cycle`. Kept in a map of this lock rather than a `ThreadLocal`: the claimant refers to this lock, and so to its `Global`,
  // and a `ThreadLocal`'s value is held strongly by a thread that outlives the compiler (I8).
  private final class LockClaimant(t: Thread) extends Claimant(t) {
    // the claim this thread is taking, for the x-ray's record of a wait
    var xrayKey: AnyRef = null
    var xraySubject: AnyRef = null
    override def canAbort = false
    override def recordWait(nanos: Long, holderWhat: String, holderName: String): Unit = {
      waitNanos.add(nanos); waits.increment(); PerThreadTimes.addWait(nanos)
      val xray = ParallelTyperTimeline.current; if (xray ne null) { val t1 = System.nanoTime(); xray.waited(xrayKey, xraySubject, t1 - nanos, t1) } // xray
    }
  }
  private[this] val claimants = new java.util.concurrent.ConcurrentHashMap[Thread, LockClaimant]
  // every claimant, for the x-ray's test of whether a released hold was waited on
  @volatile private[this] var allClaimants = new Array[LockClaimant](0)
  private[this] val newClaimant: java.util.function.Function[Thread, LockClaimant] = { t =>
    val c = new LockClaimant(t)
    synchronized { allClaimants = allClaimants :+ c }
    c
  }
  private def claimant: LockClaimant = {
    val t = Thread.currentThread
    val c = claimants.get(t)
    if (c ne null) c else claimants.computeIfAbsent(t, newClaimant)
  }
  private[this] val claims: PerKeyClaims =
    new PerKeyClaims(() => claimant, new CycleHandler { def waitCycle(s: AnyRef): Nothing = cycle(s) })

  /** `claims.acquire`, and when the x-ray is recording, the claim's key and `subject` for a wait, and with `hold` the
   *  start of an outermost hold. */
  private def acquireClaim(key: AnyRef, what: What, subject: AnyRef, onCycle: () => Nothing, hold: Boolean): Unit = {
    val xray = ParallelTyperTimeline.current // xray
    if (xray eq null) claims.acquire(key, what, onCycle)
    else {
      val c = claimant
      c.xrayKey = key; c.xraySubject = subject
      claims.acquire(key, what, onCycle)
      c.xrayKey = null; c.xraySubject = null
      if (hold && claims.holdCount(key) == 1) xray.holdStart(key, subject)
    }
  }
  /** `claims.release`, and when the x-ray is recording, the end of an outermost hold. */
  private def releaseClaim(key: AnyRef): Unit = {
    val xray = ParallelTyperTimeline.current // xray
    if ((xray ne null) && claims.holdCount(key) == 1) {
      val contended = isWaitedOn(claimant)
      claims.release(key)
      xray.holdEnd(key, contended)
    }
    else claims.release(key)
  }
  /** Is another thread parked on a claim that `me` holds? */
  private def isWaitedOn(me: Claimant): Boolean = {
    val cs = allClaimants
    var i = 0
    while (i < cs.length) {
      val w = cs(i).waitingOn
      if ((w ne null) && (w.owner eq me)) return true
      i += 1
    }
    false
  }

  /** `body` holding the claim on `key`, to do `what`; a wait-for cycle on the way is reported against `subject`. */
  def claim[T](key: AnyRef, what: What, subject: AnyRef)(body: => T): T = {
    acquireClaim(key, what, subject, () => cycle(subject), hold = true)
    if (claims.ownedHere(key)) acquisitions.increment()
    try body finally releaseClaim(key)
  }

  /** Claim `key` to complete `subject`; a wait that would close a wait-for cycle calls `onCycle`. */
  def acquireKey(key: AnyRef, what: What, subject: AnyRef, onCycle: () => Nothing): Unit = {
    acquireClaim(key, what, subject, onCycle, hold = true)
    acquisitions.increment()
  }
  def releaseKey(key: AnyRef): Unit = releaseClaim(key)
  /** Who holds what, for a report of a wait-for cycle. */
  def holders: String = claims.holdersNow

  /** Wait until no other thread holds the claim on `key`. A wait that would close a wait-for cycle calls `onCycle`
   *  (which throws). */
  def awaitKey(key: AnyRef, subject: AnyRef, onCycle: () => Nothing = null): Unit = {
    acquireClaim(key, CompletionLock.waiting, subject, if (onCycle ne null) onCycle else () => cycle(subject), hold = false)
    claims.release(key)
  }
  private[this] var holdStart = 0L

  val waitNanos = new LongAdder
  val waits = new LongAdder
  val holdNanos = new LongAdder
  val acquisitions = new LongAdder

  def acquire(): Unit = acquire(CompletionLock.completing)

  /** Claim the coarse root to do `what` (a site label for the reports of waiting threads). */
  def acquire(what: What): Unit = {
    acquireClaim(CompletionLock.CoarseRoot, what, null, () => cycle(CompletionLock.CoarseRoot), hold = true)
    if ((sites ne null) && claims.holdCount(CompletionLock.CoarseRoot) == 1) { recordSite(); holdStart = System.nanoTime() }
  }

  /** `-Dscalac.parallelTyper.lockSites`: count the outermost acquisitions by the first caller outside the lock's
   *  plumbing (slow: a stack walk per acquisition). */
  private[this] val sites: java.util.concurrent.ConcurrentHashMap[String, LongAdder] =
    if (System.getProperty("scalac.parallelTyper.lockSites") != null) new java.util.concurrent.ConcurrentHashMap else null
  private[this] val siteHolds = new java.util.concurrent.ConcurrentHashMap[String, LongAdder]
  private[this] var holdSite: String = null
  private def recordSite(): Unit = {
    val frames = new Throwable().getStackTrace
    val plumbing = Set("acquire", "apply", "underCompletionLock", "completeInfoUnderLock", "await", "awaitCompletionElsewhere", "onCompletionLock", "frontendSynch")
    val f = frames.find(f => !plumbing(f.getMethodName) && !f.getClassName.endsWith("CompletionLock")).getOrElse(frames.last)
    // the worker's name gives the phase, on the pool
    val site = s"${Thread.currentThread.getName.replaceAll("-[0-9]+$", "")} ${f.getClassName}.${f.getMethodName}"
    sites.computeIfAbsent(site, _ => new LongAdder).increment()
    holdSite = site
  }
  def siteSummary: String =
    if (sites eq null) ""
    else {
      import scala.jdk.CollectionConverters._
      sites.asScala.toList.map { case (k, v) => (k, v.sum, siteHolds.getOrDefault(k, new LongAdder).sum / 1000000) }.sortBy(-_._3).take(40)
        .map { case (k, v, ms) => f"$v%10d $ms%6d ms $k" }.mkString("\n", "\n", "")
    }

  def release(): Unit = {
    if ((sites ne null) && claims.holdCount(CompletionLock.CoarseRoot) == 1 && (holdSite ne null))
      siteHolds.computeIfAbsent(holdSite, _ => new LongAdder).add(System.nanoTime() - holdStart)
    releaseClaim(CompletionLock.CoarseRoot)
  }

  @inline def apply[T](body: => T): T = {
    acquire()
    try body
    finally release()
  }

  /** Wait until no other thread holds the coarse root: what another thread was completing under it is then done. */
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

object CompletionLock {
  /** The label of a completion's acquisition. */
  val completing: What = new What("completing a symbol", null)
  /** The label of a wait for another thread's completion. */
  val waiting: What = new What("waiting for a completion", null)
  /** The claim of everything that is not a source symbol's completion: the classpath (loaders, unpickler, classfile
   *  parser, which share state across files), the type caches still under the lock, macro expansion. Coarse. */
  object CoarseRoot { override def toString = "the coarse root" }
  val coarse: What = new What("the classpath and shared caches", null, coarse = true)
  /** Thrown by a wait for another thread's completion that would close a wait-for cycle, to the waiter, which then
   *  reads a provisional info if the completion has published one (as a serial nesting would), or reports the cycle. */
  object WaitCycle extends scala.util.control.ControlThrowable
}
