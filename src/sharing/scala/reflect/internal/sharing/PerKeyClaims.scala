package scala.reflect.internal.sharing

import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.CollectionHasAsScala

/** A `ClaimRegistry` for parallel threads: a claim per key, so that work on different keys runs at once.
 *
 *  A claim is its claimant's: reentrant for that claimant, and every other claimant that needs the key waits until
 *  the owner releases it, then looks at what the key guards again (computed, or left computable by an owner that
 *  unwound).
 *
 *  A wait that would close a cycle in the wait-for graph (each waiting claimant records the claim it waits on,
 *  `Claimant.waitingOn`; following the owners from the claim leads back to the waiter) is a dependency cycle, handled
 *  as `Claimant` says. The check runs at every slice of a wait, since another claimant's wait may start after this
 *  one's.
 *
 *  @param currentClaimant      the claimant of the calling thread
 *  @param cycles               what a claim does with a wait-for cycle it reports
 *  @param coarseCycleWaitNanos how long a claimant that cannot abort waits on a cycle through a coarse claim
 *                              (`What.coarse`) before it aborts after all; 0 for never
 */
final class PerKeyClaims(currentClaimant: () => Claimant, cycles: CycleHandler, coarseCycleWaitNanos: Long = 0L) extends ClaimRegistry {
  import PerKeyClaims.Claim

  def this(currentClaimant: () => Claimant) = this(currentClaimant, AbortVictim)

  def claim[T](key: AnyRef, kind: Sharing.ClaimKind[Nothing], cycleSubject: AnyRef, op: () => T): T = {
    acquire(key, kind.what, () => cycles.waitCycle(cycleSubject))
    try op() finally release(key)
  }

  private val claims = new ConcurrentHashMap[AnyRef, Claim]()

  /** Claim `key` for this thread's claimant, to do `what`; wait while another claimant holds it. `cycle` reports a
   *  wait-for cycle (it throws).
   */
  def acquire(key: AnyRef, what: What, cycle: () => Nothing): Unit = {
    val me = currentClaimant()
    var waitedSince = 0L
    var seen: Claim /* | Null */ = null
    var cycleSince = 0L
    var lastChain: List[Claim] /* | Null */ = null
    while (true) {
      val c = claims.get(key)
      if (c == null) {
        val mine = new Claim(me, what, Thread.currentThread.getName)
        if (claims.putIfAbsent(key, mine) == null) {
          me.acquired()
          if (waitedSince != 0L) {
            val s = seen
            me.recordWait(System.nanoTime() - waitedSince, s.what.render, s.ownerName)
          }
          return
        }
      } else if (c.owner eq me) {
        c.holds += 1
        return
      } else {
        if (waitedSince == 0L) {
          waitedSince = System.nanoTime()
          seen = c
        }
        cycleChain(me, c) match {
          case null =>
            cycleSince = 0L
            lastChain = null
          case chain =>
            val parties = chain.map(_.owner)
            if (me.canAbort) me.abort()
            else if (parties.exists(_.canAbort)) {
              () // a party that can abort does so at its next look: wait
            } else if (coarseCycleWaitNanos > 0L && chain.exists(_.what.coarse)) {
              // through a coarse claim: its owner likely finishes without what this claimant holds; wait, bounded
              if (cycleSince == 0L) cycleSince = System.nanoTime()
              else if (System.nanoTime() - cycleSince > coarseCycleWaitNanos) me.abort()
            } else if (parties.forall(p => System.identityHashCode(p.thread) <= System.identityHashCode(me.thread))) {
              // one party reports the cycle (a fixed order of the threads picks it). The chain is read from fields
              // the claimants change as they go: only one seen at two looks, a slice apart, with the same claims, is
              // a cycle and not a snapshot of two waits that ended
              val last = lastChain
              if (last != null && last.corresponds(chain)(_ eq _)) cycle()
              else lastChain = chain
            }
        }
        me.waitingOn = c
        try c.done.await(5, TimeUnit.MILLISECONDS)
        finally me.waitingOn = null
      }
    }
  }

  /** Release a claim made by `acquire` on this thread. */
  def release(key: AnyRef): Unit = {
    val c = claims.get(key)
    c.holds -= 1
    if (c.holds == 0) {
      claims.remove(key)
      c.done.countDown()
    }
  }

  /** Run `op` holding the claim on `key`. */
  def locked[T](key: AnyRef, what: What, cycle: () => Nothing)(op: => T): T = {
    acquire(key, what, cycle)
    try op finally release(key)
  }

  /** Does this thread's claimant hold the claim on `key`? */
  def ownedHere(key: AnyRef): Boolean = {
    val c = claims.get(key)
    c != null && (c.owner eq currentClaimant())
  }

  /** The claims of a wait-for cycle that waiting on `c` would close: `c`, then what its owner waits on, and so on,
   *  back to a claim of `me`; null if the owners do not lead back to `me`.
   */
  private def cycleChain(me: Claimant, c: Claim): List[Claim] /* | Null */ = {
    var claim: Claim /* | Null */ = c
    var chain: List[Claim] = Nil
    var steps = 0
    while (claim != null && steps < 1000) {
      // a released claim is a stale edge: `waitingOn` of a claimant that has been woken and has not yet cleared it
      if (claim.done.getCount == 0) return null
      chain = claim :: chain
      if (claim.owner eq me) {
        // the edges were read one after another: a claim released after it was read, whose owner then waited on a
        // claim of `me`, joins two waits that never coexisted. Claims are not reacquired, so every claim still held
        // now was held throughout the walk, and the waits on them still stand
        return if (chain.forall(_.done.getCount != 0)) chain else null
      }
      claim = claim.owner.waitingOn
      steps += 1
    }
    null
  }

  /** How many holds this thread's claimant has on `key`. */
  def holdCount(key: AnyRef): Int = {
    val c = claims.get(key)
    if (c != null && (c.owner eq currentClaimant())) c.holds else 0
  }

  /** The claims held now, for diagnostics. */
  def holdersNow: String =
    claims.values.asScala.map(c => s"${c.ownerName}: ${c.what.render}").mkString("; ")
}

object PerKeyClaims {
  /** A claimant's claim on a key, released when its holds come back to 0. */
  final class Claim(val owner: Claimant, val what: What, val ownerName: String) {
    // changed only by the owner (`acquire` re-entering, `release`)
    @volatile var holds: Int = 1
    val done: CountDownLatch = new CountDownLatch(1)
  }
}
