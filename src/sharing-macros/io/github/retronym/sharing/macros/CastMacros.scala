package io.github.retronym.sharing.macros

import scala.reflect.macros.blackbox

/** Into and out of the types that Scala 3 declares opaque: casts, which erase to nothing (or to a box, for a primitive
 *  in an abstract type), with the evidence they take not evaluated. */
class CastMacros(val c: blackbox.Context) extends Common {
  import c.universe._

  def identity[T: c.WeakTypeTag](t: Tree): Tree = t
  def cast[R: c.WeakTypeTag](t: Tree): Tree = q"$t.asInstanceOf[${weakTypeOf[R]}]"
  /** `Claimed(t)`: of the claim `h` is evidence of, as the macro is expanded before the type arguments that the
   *  expected type gives are inferred. */
  def claimed(t: Tree)(h: Tree): Tree = {
    val k = h.tpe.widen.typeArgs.head
    q"$t.asInstanceOf[${ClaimedType(k, t.tpe.widen)}]"
  }
}

/** The read of a `Claimed` or `Guarded` value, `c()`: the value, once an implicit search finds the evidence. */
class ClaimedMacros(val c: blackbox.Context) extends Common {
  import c.universe._

  private def readWith(evidence: Type, missing: String): Tree = {
    if (c.inferImplicitValue(evidence, silent = true).isEmpty) c.abort(c.enclosingPosition, missing)
    prefixArg
  }

  def read[K: c.WeakTypeTag, T: c.WeakTypeTag](): Tree = {
    val k = weakTypeOf[K]
    q"${readWith(HoldsType(k), s"this code must run as the one writer of $k: inside Sharing.claim or Sharing.once on it, Sharing.claimAt(owner) for owner: $k, or a completion of it")}.asInstanceOf[${weakTypeOf[T]}]"
  }

  def readGuarded[K: c.WeakTypeTag, T: c.WeakTypeTag](): Tree = {
    val k = weakTypeOf[K]
    q"${readWith(MonitorType(k), s"this code must hold the monitor of $k: inside its `locked` or `alwaysLocked`, if it is a Sharing.Guard, or Sharing.serialised or Sharing.locking on it")}.asInstanceOf[${weakTypeOf[T]}]"
  }
}
