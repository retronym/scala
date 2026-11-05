package scala.reflect.internal

import scala.reflect.internal.sharing.macros.{ClaimedMacros, OnceMacros, PublishedMacros, PublishedWhiteboxMacros}

/** The Scala 2.13 build: the types that Scala 3 declares as opaque, and the operations on them.
 *
 *  An opaque type is an abstract type here, cast to and from what it holds. Scala 2 erases an abstract type to
 *  `Object`, so a `Claimed`, `Guarded`, `Payload` or `Acquired` of a primitive boxes it; of a reference, it costs a
 *  `checkcast` on a read. `Published[T]` is an alias of `T` instead, so that a published `Int` stays an `int`: its
 *  plain reads and stores type-check, and only its operations check that the field is declared `Published`.
 *
 *  The operations are macros in implicit classes, found by code in this package or that imports it
 *  (`import scala.reflect.internal.sharing._`). Their expansions never construct the implicit class.
 */
package object sharing {
  import Sharing.{ClaimKind, Holds, Monitor}
  import Published.{Acquired, Publishing}

  /** A field that publishes others: written with release, read with acquire semantics. See the Scala 3 `Published`.
   *
   *  An alias of `T`, so that the field keeps `T`'s erasure: an abstract type would box a primitive. */
  type Published[T] = T

  /** A field published by its owner's [[Published]] field `F`. See the Scala 3 `Payload`. */
  type Payload[O, F <: String, T]

  /** A field holding a lazy fact, computed once by the one writer of its owner `K`: `null` until stored. */
  type Once[K, T <: AnyRef] <: AnyRef

  /** A value reached only with the evidence of a claim on `K`, [[Sharing.Holds]]. */
  type Claimed[K, +T]

  /** A value of a [[Sharing.Guard]] reached only with the evidence of the monitor of `K`, the guard. */
  type Guarded[K, +T]

  /** The operations on a [[Published]] field, `c.flag.acquire`: on any value, since `Published[T]` is `T`; the
   *  macros reject a receiver that is not a `var` declared `Published[T]`. */
  implicit class PublishedOps[T](field: T) {
    /** The field, loaded with acquire semantics. */
    def acquire: T = macro PublishedMacros.acquire[T]
    /** Stores `v` in the field with release semantics. */
    def release(v: T): Unit = macro PublishedMacros.release[T]
    /** A plain read: only where the protocol allows it. */
    def plain: T = macro PublishedMacros.plain[T]
    /** A plain store: only where the protocol allows it. */
    def setPlain(v: T): Unit = macro PublishedMacros.setPlain[T]
    /** The field, loaded with acquire semantics, as an `Acquired[owner.type, "field", T]`. */
    def acquired: Any = macro PublishedWhiteboxMacros.acquired[T]
    /** Runs `body`, which stores this field's payloads (`p := v`), then stores `v` with release semantics:
     *  `c.flag.publishing(1) { implicit p => c.payload := 42 }`. */
    def publishing(v: T)(body: Publishing => Unit): Unit = macro PublishedMacros.publishing[T]
  }

  implicit class AcquiredOps[S, F <: String, T](snap: Acquired[S, F, T]) {
    /** The publisher's value, as acquired. */
    def value: T = macro PublishedMacros.acquiredValue[T]
  }

  implicit class PayloadOps[O, F <: String, T](field: Payload[O, F, T]) {
    /** The field, read after `snap`, the acquire of its publisher on the same object. */
    def after[S, V](snap: Acquired[S, F, V]): T = macro PublishedMacros.after[S, T]
    /** A plain read, with no order: only where the protocol allows it. */
    def plain: T = macro PublishedMacros.payloadPlain[T]
    /** Stores `v` in the field, inside a `publishing` block of its publisher. */
    def :=(v: T)(implicit p: Publishing): Unit = macro PublishedMacros.payloadStore[O, F, T]
  }

  implicit class OnceOps[K <: AnyRef with Singleton, T <: AnyRef](field: Once[K, T]) {
    /** The fact, or, when there is none, the value of `compute`, run as the one writer of the field's owner and
     *  stored: `myInfo.get { implicit h => computeInfo() }`. See the Scala 3 `Once.get`. */
    def get(compute: Holds[K] => T)(implicit kind: ClaimKind[K], mode: Mode, registry: ClaimRegistry): T =
      macro OnceMacros.get[K, T]
    /** The fact if it is there, else `null`, without claiming. */
    def peek(implicit mode: Mode): T = macro OnceMacros.peek[T]
    /** The fact if it is there, else `null`, read with acquire semantics whatever the mode. */
    def acquire: T = macro OnceMacros.acquire[T]
    /** Stores the fact as the one writer, outside `get`. */
    def set(v: T)(implicit h: Holds[K], mode: Mode): Unit = macro OnceMacros.set[K, T]
    /** Forgets the fact, as the one writer, so that the next `get` computes it again. */
    def reset()(implicit h: Holds[K], mode: Mode): Unit = macro OnceMacros.reset[K, T]
    /** A plain store: only where the protocol allows it. */
    def setPlain(v: T): Unit = macro OnceMacros.setPlain[K, T]
  }

  // `c()` takes its evidence by an implicit search in the macro, not as an implicit parameter, so that `c()(i)`
  // applies the value, as it does in Scala 3, rather than passing `i` as the evidence

  implicit class ClaimedOps[K, T](c: Claimed[K, T]) {
    /** The value, as the one writer: with the evidence `Holds[K]` in implicit scope. */
    def apply(): T = macro ClaimedMacros.read[K, T]
  }

  implicit class GuardedOps[K, T](g: Guarded[K, T]) {
    /** The value, holding the guard's monitor: with the evidence `Monitor[K]` in implicit scope. */
    def apply(): T = macro ClaimedMacros.readGuarded[K, T]
  }
}
