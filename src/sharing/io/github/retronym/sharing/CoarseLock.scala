package io.github.retronym.sharing

import java.util.concurrent.locks.ReentrantLock

/** A `ClaimRegistry` for serialised threads: one reentrant lock, taken by a claim whose kind says so
 *  (`ClaimKind.lockWhenSerialised`), so that one thread computes at a time.
 *
 *  It also keeps what its holder is doing (the outermost acquisition, the root of the holder's work, and the
 *  innermost), so that a thread that has to wait can report how long it waited and what for (`onWait`). The labels
 *  are kept as objects and rendered only by a thread that waited.
 *
 *  @param onWait called by a thread that had to wait, once it has the lock, with the nanoseconds it waited, what the
 *                holder's root and innermost work were when the wait began, and the holder's thread name
 */
final class CoarseLock(onWait: (Long, String, String, String) => Unit = (_, _, _, _) => ()) extends ReentrantLock with ClaimRegistry {

  def claim[T](key: AnyRef, kind: Sharing.ClaimKind[Nothing], cycleSubject: AnyRef, op: () => T): T =
    if (kind.lockWhenSerialised) {
      lockFor(kind.what)
      try op() finally unlockFor()
    } else op()

  // written by the holder only; read by waiters before they block
  @volatile private var rootWhat: What /* | Null */ = null
  @volatile private var currentWhat: What /* | Null */ = null
  @volatile private var holderName: String = ""
  // the holder's, touched only between `lockFor` having acquired the lock and `unlockFor` releasing it
  private val stack = new java.util.ArrayDeque[What]()

  /** Acquire the lock to do `what`. */
  def lockFor(what: What): Unit = {
    if (!tryLock()) {
      // rendered when the wait begins, while the holder is still doing it (contended acquisitions only)
      val root = String.valueOf(rootWhat)
      val current = String.valueOf(currentWhat)
      val holder = holderName
      val t0 = System.nanoTime()
      lock()
      onWait(System.nanoTime() - t0, root, current, holder)
    }
    if (getHoldCount == 1) {
      rootWhat = what
      holderName = Thread.currentThread.getName
    }
    stack.push(what)
    currentWhat = what
  }

  /** Release an acquisition made by `lockFor`. */
  def unlockFor(): Unit = {
    stack.pop()
    currentWhat = stack.peek()
    if (getHoldCount == 1) {
      rootWhat = null
      holderName = ""
    }
    unlock()
  }

  /** Who holds the lock and the root of what it is doing, or "". */
  def holderNow: String = {
    val r = rootWhat
    if (r == null) "" else s"$holderName: ${r.render}"
  }

  /** Run `op` holding the lock for `what`. */
  def locked[T](what: What)(op: => T): T = {
    lockFor(what)
    try op finally unlockFor()
  }
}
