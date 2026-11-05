package io.github.retronym.sharing

import io.github.retronym.sharing.macros.{CastMacros, PublishedMacros}

/** The companion of [[Published]], an alias in the Scala 2.13 build: see the package object. A field and its handle:
 *
 *  {{{
 *  class Entry {
 *    private[mypkg] var version: Published[Int] = Published(0)
 *  }
 *  object Entry {
 *    implicit val versionHandle: Published.Handle[Entry, "version"] = Published.handle
 *  }
 *  }}}
 *
 *  An implicit `val` is a field, so the handle is the val itself (no alias given, which Scala 3 needs to avoid a lazy
 *  val).
 */
object Published {

  /** The field's initial value, stored before its owner is shared. Expands to `t`. */
  def apply[T](t: T): Published[T] = macro CastMacros.identity[T]

  /** The `VarHandle` of field `F` of class `O`, declared once, as an implicit in `O`'s companion. Bounded by `AnyRef`,
   *  not `VarHandle`, so that only `Publish` names the JDK 9 class: it is a `VarHandle` at run time. */
  type Handle[O, F <: String] <: AnyRef

  /** The handle of field `F` of class `O`, looked up when the companion is initialised. A macro, so that a field that is
   *  not there, or not a `Published` or a `Once`, is a compile error. */
  def handle[O, F <: String]: Handle[O, F] = macro PublishedMacros.handle[O, F]

  /** The value of `S`'s publisher `F`, loaded with acquire semantics: the evidence that a [[Payload]] of `F` is read
   *  after it. Boxed if `T` is a primitive. */
  type Acquired[S, F <: String, T]

  /** Evidence that a store runs inside a `publishing` block, before the release that ends it. */
  type Publishing <: AnyRef
}

object Payload {
  /** The field's initial value, stored before its owner is shared. */
  def apply[O, F <: String, T](t: T): Payload[O, F, T] = t.asInstanceOf[Payload[O, F, T]]
}
