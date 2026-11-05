package io.github.retronym.sharing

/** A party that holds and waits for claims in `PerKeyClaims`: one per thread, or one per request that may move
 *  between threads. `PerKeyClaims` compares claimants by identity.
 *
 *  `PerKeyClaims` detects deadlock on the wait-for graph, and picks a victim. When waiting for a claim would close a
 *  cycle (another claimant holds the claim, and is waiting, directly or through others, for one of this claimant's):
 *
 *  - a claimant that `canAbort` is the victim: it throws `Cancelled` at once, so its claims unwind and its work is
 *    retried;
 *  - otherwise, while some party of the cycle can abort, it waits for that one to;
 *  - otherwise, if a claim of the cycle is coarse (`What.coarse`) and the registry bounds such a wait, it waits up to
 *    the bound for the cycle to clear, and then aborts after all;
 *  - otherwise one party of the cycle (the same for every party, by a fixed order of their threads) reports it to the
 *    claim's `cycle` (a `CycleHandler`), once it has seen the same cycle at two looks.
 *
 *  This is detection, as in a database's lock manager, not prevention: wound-wait and wait-die (Rosenkrantz, Stearns
 *  and Lewis, 1978) order the parties by a timestamp so that no cycle forms, which would abort work that a cycle never
 *  threatened.
 *
 *  In the compiler a claimant typing a method body can abort (its request is retried), and one inside a signature's
 *  completion cannot: a cycle between signatures is a cyclic reference, reported as an error.
 *
 *  @param thread the thread whose identity orders the parties of a cycle
 */
class Claimant(val thread: Thread = Thread.currentThread) {
  /** The claim this claimant is waiting for, or null: the edges of the wait-for graph. */
  @volatile var waitingOn: PerKeyClaims.Claim /* | Null */ = null

  /** How many times this claimant was aborted as the victim of a wait-for cycle. */
  @volatile var aborted: Int = 0

  /** Whether this claimant can be the victim of a wait-for cycle, aborted by throwing `Cancelled`. Read at every look
   *  of a wait. */
  def canAbort: Boolean = true

  /** Aborts this claimant as the victim of a wait-for cycle: counts it and throws, so that its claims unwind and its
   *  work is retried. Override to throw your own subclass of `Cancelled`, the one your work's restart catches. */
  def abort(): Nothing = {
    aborted += 1
    throw new Cancelled()
  }

  /** Called once the claimant has taken a claim, before the claimed code runs: a place for a test's schedule
   *  noise. Does nothing by default. */
  def acquired(): Unit = ()

  /** Called once the claimant has the claim it waited `nanos` for, with what the holder was doing when the wait began
   *  and who it was. For lock-wait statistics; does nothing by default. */
  def recordWait(nanos: Long, holderWhat: String, holderName: String): Unit = ()
}

/** A `CycleHandler` that aborts the thread that reports the cycle, as the victim: throws `Cancelled`, as a claimant
 *  that can abort does. */
object AbortVictim extends CycleHandler {
  def waitCycle(subject: AnyRef): Nothing = throw new Cancelled()
}
