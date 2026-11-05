package scala.reflect.internal.sharing

/** Who holds the claims on keys when threads are concurrent. A claim is a reentrant lock per key, whose waits feed a
 *  wait-for graph for deadlock detection ([[PerKeyClaims]]), or the one coarse lock ([[CoarseLock]]).
 *
 *  [[Sharing.claim]], [[Sharing.claimAt]] and [[Sharing.once]] call it in [[Mode.Concurrent]] (never in
 *  [[Mode.Batch]]). The owner of the shared state supplies one as a `given`, beside the [[Mode]]. The library
 *  states the contract, and provides the two shapes that fit it:
 *
 *  - **One coarse lock**, a [[CoarseLock]], for every key whose [[Sharing.ClaimKind]] says serialised threads take it: threads are
 *    serialised, and a claim is the lock.
 *  - **A lock per key**, [[PerKeyClaims]], for parallel threads. Threads that claim different keys run side by side. A thread that
 *    would wait for a claim whose holder is, directly or through other claims, waiting for it closes a wait-for
 *    cycle. The registry reports that to a [[CycleHandler]] instead of deadlocking.
 *
 *  The contract: at most one thread holds the claim on a key at a time, and the holder claiming the same key again
 *  (re-entry) runs at once instead of waiting for itself.
 */
trait ClaimRegistry {
  /** Runs `op` as the one writer of the lazy facts keyed by `key`: holding the claim on it, or the coarse lock if
   *  `kind` says serialised threads take it, or as is if they need no claim. Re-entry by the holder runs `op` at
   *  once.
   *
   *  A wait that would close a wait-for cycle is reported against `cycleSubject` (to a [[CycleHandler]]).
   *
   *  @param key          identity of the claim (compared by reference, so use a stable object)
   *  @param kind         how this kind of key is claimed, from the key's [[Sharing.ClaimKind]]
   *  @param cycleSubject what to report if waiting for the claim would close a cycle
   *  @param op           the code to run as the one writer
   */
  def claim[T](key: AnyRef, kind: Sharing.ClaimKind[Nothing], cycleSubject: AnyRef, op: () => T): T
}

/** What a claim does when waiting for it would close a wait-for cycle between threads.
 *
 *  It throws, so that the waiting thread unwinds and releases the claims it holds, and the work it was doing can be
 *  restarted after the other threads have finished. A common choice is a [[Cancelled]] caught where the work begins.
 */
trait CycleHandler {
  /** Called on the thread that would close the cycle. Does not return.
   *
   *  @param subject what the thread was waiting for, as the `cycleSubject` of the claim
   */
  def waitCycle(subject: AnyRef): Nothing
}

/** A thread's work unwinding because it was cancelled, or aborted as the victim of a wait-for cycle (a
 *  [[CycleHandler]] may throw one).
 *
 *  Not a failure of what the thread was computing: that is left as it was found, to be computed by the next caller,
 *  and the exception is caught where the work restarts. A [[scala.util.control.ControlThrowable]], so
 *  `scala.util.control.NonFatal` does not catch it by accident.
 */
class Cancelled extends scala.util.control.ControlThrowable

/** What the holder of a claim is doing: a kind and an optional subject.
 *
 *  Rendered only when a waiter reports it, for example in a lock-wait diagnostic ("waiting for completing class Foo"),
 *  so building a `What` costs next to nothing. The subject is a `String`, a [[Label]] (computed when reported), or
 *  any object, which [[renderSubject]] renders; override that to describe your own types.
 *
 *  @param kind    a short phrase for the activity
 *  @param subject what the activity is on, or `null`
 *  @param coarse  whether the claim guards much more than any one waiter needs (a whole file's lazy reads, the
 *                 inherited members of a class): a wait-for cycle through it is then likely a false dependency, which
 *                 [[PerKeyClaims]] breaks after a bound even when no party can abort
 */
class What(val kind: String, val subject: AnyRef /* | Null */, val coarse: Boolean = false) {
  /** The activity as text, for a report. */
  final def render: String = subject match {
    case null => kind
    case str: String => s"$kind $str"
    case l: Label => s"$kind ${l.text}"
    case other => s"$kind ${renderSubject(other)}"
  }
  /** Renders a subject that is not a `String` or a [[Label]]. The default is its class's simple name. */
  protected def renderSubject(subject: AnyRef): String = subject.getClass.getSimpleName
  override def toString: String = render
}

/** A subject computed when a waiter reports it, such as what the holder is doing by then.
 *
 *  @param f produces the text; called only when the report is made
 */
final class Label(f: () => String) {
  /** The text, computed now. */
  def text: String = f()
}
