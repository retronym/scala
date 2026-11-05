package scala.reflect.internal.sharing

import scala.reflect.internal.sharing.macros.CastMacros

/** The companion of [[Claimed]], an abstract type in the Scala 2.13 build: see the package object.
 *
 *  {{{
 *  private var rootOwner: Claimed[this.type, Symbol] = Claimed.initial(NoSymbol)
 *  def enterQuote(owner: Symbol)(implicit h: Sharing.Holds[this.type]): Unit = rootOwner = Claimed(owner)
 *  }}}
 */
object Claimed {
  /** The value of a field in its initialiser, before its owner is shared, with no evidence. */
  def initial[K, T](t: T): Claimed[K, T] = t.asInstanceOf[Claimed[K, T]]

  /** The value to store in a field, as the one writer: `rootOwner = Claimed(owner)`. The evidence is not evaluated. */
  def apply[K, T](t: T)(implicit h: Sharing.Holds[K]): Claimed[K, T] = macro CastMacros.claimed
}
