package io.github.retronym.sharing

import scala.collection.mutable

/** How a body of state is shared between threads, chosen when its owner is made and never changed.
 *
 *  - `Concurrency.Batch`: one thread, no locking.
 *  - `Concurrency.Serialised`: coarse-grained. Several threads and one `CoarseLock`, so one computes at a time.
 *  - `Concurrency.Parallel`: fine-grained. A claim per key (`PerKeyClaims`): one key's intermediate state is seen only
 *    by its claimant, and others wait for it.
 *
 *  The owner keeps one, and supplies the operations' givens from it: its `mode` and a registry made by `newRegistry`.
 *
 *  @param concurrent whether threads share the state
 *  @param parallel   whether they compute at once, with a claim per key
 */
final class Concurrency private (val concurrent: Boolean, val parallel: Boolean) {
  assert(concurrent || !parallel)

  /** The mode for the operations' given. */
  val mode: Mode = new Mode(concurrent)

  /** The registry for the operations' given: a `CoarseLock` when serialised, `PerKeyClaims` over `currentClaimant`
   *  when parallel; in batch mode one that is never called. */
  def newRegistry(currentClaimant: () => Claimant, cycles: CycleHandler = AbortVictim): ClaimRegistry =
    if (parallel) new PerKeyClaims(currentClaimant, cycles)
    else if (concurrent) new CoarseLock()
    else Concurrency.NoRegistry

  // ---- construction-time choices: called once per owner, cache or class, never per access

  /** A mutable map shared by the threads: a `TrieMap` when they are concurrent. */
  def newMap[K, V](): mutable.Map[K, V] =
    if (concurrent) scala.collection.concurrent.TrieMap[K, V]() else mutable.HashMap[K, V]()

  override def toString: String =
    if (parallel) "Concurrency.Parallel" else if (concurrent) "Concurrency.Serialised" else "Concurrency.Batch"
}

object Concurrency {
  val Batch: Concurrency = new Concurrency(concurrent = false, parallel = false)
  val Serialised: Concurrency = new Concurrency(concurrent = true, parallel = false)
  val Parallel: Concurrency = new Concurrency(concurrent = true, parallel = true)

  private object NoRegistry extends ClaimRegistry {
    def claim[T](key: AnyRef, kind: Sharing.ClaimKind[Nothing], cycleSubject: AnyRef, op: () => T): T =
      throw new IllegalStateException("a claim in Mode.Batch reaches no registry")
  }
}
