package io.github.retronym.sharing

import scala.annotation.implicitNotFound

import io.github.retronym.sharing.macros.SharingMacros

/** Operations on state shared between threads, with evidence types that make the guarded state reachable only from
 *  inside them: the Scala 2.13 build. The Scala 3 build's documentation of each operation holds here; this one says
 *  only where the Scala 2 form differs.
 *
 *  Each operation that takes code to run is a def macro whose batch branch is that code, as an `inline def` is in Scala
 *  3: a call site compiles to a test of the implicit [[Mode]] and, in batch mode, the code itself, with no allocation.
 *  Code run more than once in an expansion (in both branches) is a local method that each calls, since a Scala 2 macro
 *  cannot copy a typed tree that defines symbols: the batch branch calls it directly, the concurrent one from the
 *  closure it hands the registry.
 *
 *  The evidence a context function gives in Scala 3 is the parameter of a function literal, marked `implicit`:
 *
 *  {{{
 *  Sharing.claim(this) { implicit h => store(computeInfo()) }
 *  }}}
 *
 *  The macros pass the shared evidence object for it. A literal is beta-reduced; any other function value is applied.
 */
object Sharing {

  /** Evidence that the code it is given to is the one writer of the lazy facts keyed by `key: K`. */
  @implicitNotFound("this code must run as the one writer of ${K}: inside Sharing.claim or Sharing.once on it, Sharing.claimAt(owner) for owner: ${K}, or a completion of it")
  sealed abstract class Holds[K]
  // reached by the macros' expansions through a typed reference, which is not checked for access
  private[sharing] object held extends Holds[Any]

  /** Evidence of `claim(key)` without it, for an access that races on purpose; `why` says why it is safe. `why` is not
   *  evaluated. */
  def unguarded[K](why: String): Holds[K] = macro SharingMacros.unguarded[K]

  /** Evidence that the code it is given to holds the monitor of `lock: K`. */
  @implicitNotFound("this code must hold the monitor of ${K}: inside its `locked` or `alwaysLocked`, if it is a Sharing.Guard, or Sharing.serialised or Sharing.locking on it")
  sealed abstract class Monitor[K]
  private[sharing] object monitorHeld extends Monitor[Any]

  /** Evidence of `lock`'s monitor without holding it, for an access that races on purpose. `why` is not evaluated. */
  def unlocked[K](why: String): Monitor[K] = macro SharingMacros.unlocked[K]

  /** How the claim on a key of type `K` is taken: an implicit for each kind of key, in its companion. */
  @implicitNotFound("no Sharing.ClaimKind for ${K}: a key's class says how it is claimed with a given ClaimKind beside it")
  final class ClaimKind[-K](val what: What, val lockWhenSerialised: Boolean)
  object ClaimKind {
    def apply[K](what: What, lockWhenSerialised: Boolean): ClaimKind[K] = new ClaimKind[K](what, lockWhenSerialised)
  }

  /** Runs `op` as the one writer of the lazy facts keyed by `key`, with the evidence of it. Expands to
   *
   *  {{{
   *  def op$1(implicit h: Holds[K]): T = <op's body>
   *  if (mode.concurrent) registry.claim(key, kind, key, () => op$1(held)) else op$1(held)
   *  }}}
   */
  def claim[K <: AnyRef with Singleton, T](key: K)(op: Holds[K] => T)(
      implicit kind: ClaimKind[K], mode: Mode, registry: ClaimRegistry): T = macro SharingMacros.claim[K, T]

  /** [[claim]], reporting a wait-for cycle against `cycleSubject` (the key if `null`). */
  def claim[K <: AnyRef with Singleton, T](key: K, cycleSubject: AnyRef)(op: Holds[K] => T)(
      implicit kind: ClaimKind[K], mode: Mode, registry: ClaimRegistry): T = macro SharingMacros.claimReporting[K, T]

  /** [[claim]] on a key computed per call, as the one writer of `owner`'s state, with the evidence of it as
   *  `Holds[owner.type]`. `owner` is not evaluated. */
  def claimAt[K <: AnyRef with Singleton, T](owner: K)(key: AnyRef, kind: ClaimKind[Nothing], cycleSubject: AnyRef)(
      op: Holds[K] => T)(implicit mode: Mode, registry: ClaimRegistry): T = macro SharingMacros.claimAt[K, T]

  /** A lazy fact keyed by `key`, computed once: double-checked locking over `peek`. Expands to
   *
   *  {{{
   *  def op$1(implicit h: Holds[K]): T = { val v1 = peek; if (v1 ne null) v1 else <compute's body> }
   *  val v = peek
   *  if (v ne null) v
   *  else if (mode.concurrent) registry.claim(key, kind, key, () => op$1(held)) else op$1(held)
   *  }}}
   */
  def once[K <: AnyRef with Singleton, T <: AnyRef](key: K)(peek: T)(compute: Holds[K] => T)(
      implicit kind: ClaimKind[K], mode: Mode, registry: ClaimRegistry): T = macro SharingMacros.once[K, T]

  /** [[once]], reporting a wait-for cycle against `cycleSubject` (the key if `null`). */
  def once[K <: AnyRef with Singleton, T <: AnyRef](key: K, cycleSubject: AnyRef)(peek: T)(compute: Holds[K] => T)(
      implicit kind: ClaimKind[K], mode: Mode, registry: ClaimRegistry): T = macro SharingMacros.onceReporting[K, T]

  /** The black hole of a computation that the holder of the claim on this object runs: which thread is inside it.
   *
   *  `inside(op)` expands to `val was = blackHoleEnter(); try op finally blackHoleLeave(was)`, and
   *  `computeInside(onCycle)(op)` to `if (isInside) onCycle else inside(op)`.
   */
  trait BlackHole {
    @benignRace("written under the claim; read without it only to compare with this thread")
    private[this] var insideThread: Thread = null

    /** Runs `op` with this thread inside, restoring who was inside before (a nested entry by the same thread). */
    protected final def inside[T](op: => T)(implicit h: Holds[this.type]): T = macro SharingMacros.inside[T]

    /** Runs `op` as the one computation this black hole guards; `onCycle` if the holder re-enters it. */
    protected final def computeInside[T](onCycle: => Nothing)(op: => T)(implicit h: Holds[this.type]): T =
      macro SharingMacros.computeInside[T]

    /** Is some thread inside? */
    protected final def isInside(implicit h: Holds[this.type]): Boolean = insideThread != null

    /** Is this thread inside? */
    protected final def isInsideHere: Boolean = insideThread eq Thread.currentThread

    /** Enters, returning who was inside: for the expansion of `inside`. */
    protected final def blackHoleEnter(): Thread = {
      val was = insideThread
      insideThread = Thread.currentThread
      was
    }

    /** Leaves, restoring who was inside: for the expansion of `inside`. */
    protected final def blackHoleLeave(was: Thread): Unit = insideThread = was
  }

  /** Runs `op` holding `lock`'s monitor in a concurrent mode, and as is otherwise, with the evidence of it. Expands to
   *  `if (mode.concurrent) lock.synchronized(op$1(m)) else op$1(m)`, `op$1` a local method of `op`'s body. */
  def serialised[K <: AnyRef with Singleton, T](lock: K)(op: Monitor[K] => T)(implicit mode: Mode): T =
    macro SharingMacros.serialised[K, T]

  /** Runs `op` holding `lock`'s monitor in every mode, with the evidence of it. */
  def locking[K <: AnyRef with Singleton, T](lock: K)(op: Monitor[K] => T): T = macro SharingMacros.locking[K, T]

  /** A variable of a [[Guard]] reached only with the evidence of the monitor of `K`, the guard: `v()` and `v() = x`.
   *  A box, one object per variable. */
  final class GuardedVar[K, T](private[this] var value: T) {
    def apply()(implicit m: Monitor[K]): T = value
    def update(v: T)(implicit m: Monitor[K]): Unit = value = v
  }

  /** An object whose monitor guards the state it makes with `guarded` and `guardedVar`, reached only inside `locked`
   *  or `alwaysLocked`: `locked { implicit m => bySource()... }`. */
  abstract class Guard {
    protected final def guarded[T](init: T): Guarded[this.type, T] = init.asInstanceOf[Guarded[this.type, T]]
    protected final def guardedVar[T](init: T): GuardedVar[this.type, T] = new GuardedVar(init)
    final def locked[T](op: Monitor[this.type] => T)(implicit mode: Mode): T = macro SharingMacros.locked[T]
    final def alwaysLocked[T](op: Monitor[this.type] => T): T = macro SharingMacros.alwaysLocked[T]
  }
}
