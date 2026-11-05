package io.github.retronym.sharing

import io.github.retronym.sharing.macros.StampedMacros

/** A value with the stamp it was computed at, as one immutable object, held in one `var` of the cache's owner. See the
 *  Scala 3 build's documentation; the operations here take the current stamp as an implicit, after the computation:
 *
 *  {{{
 *  private var exports: Stamped[PerGeneration, List[String]] = Stamped.Empty
 *  def exportedNames(implicit now: Stamp[PerGeneration]): List[String] = exports.cached(computeExports())
 *  }}}
 *
 *  @tparam K the kind of the stamp (a phantom type)
 *  @tparam T the type of the value
 *  @param stamp the stamp the value was computed at
 *  @param value the value
 */
final class Stamped[+K, +T](val stamp: Int, val value: T) {
  override def toString: String = s"Stamped($stamp, $value)"
}

/** The current stamp of kind `K`: the implicit that the operations of [[Stamped]] take. A value class, so an implicit
 *  def that makes one is the `Int` it reads.
 *
 *  {{{
 *  implicit def stamp(implicit g: Generation): Stamp[PerGeneration] = new Stamp(g.current)
 *  }}}
 */
final class Stamp[K](val value: Int) extends AnyVal

object Stamped {

  /** The stamped value of a cache not yet computed: stamp `0`, which is never current. */
  val Empty: Stamped[Nothing, Nothing] = new Stamped[Nothing, Null](0, null).asInstanceOf[Stamped[Nothing, Nothing]]

  implicit class StampedOps[K, T](field: Stamped[K, T]) {

    /** The value of the cache in `field`, a `var` of its owner, valid while its stamp is the current `Stamp[K]`; on a
     *  miss, `compute`d and stored with that stamp. Expands to
     *
     *  {{{
     *  val n = now.value
     *  val c = field
     *  if (c.stamp == n) c.value
     *  else { val v = compute; field = new Stamped(n, v); v }
     *  }}}
     */
    def cached(compute: => T)(implicit now: Stamp[K]): T = macro StampedMacros.cached[K, T]

    /** Like [[cached]], but the value computed is stored only if `keep` holds of it. */
    def cachedIf(keep: T => Boolean)(compute: => T)(implicit now: Stamp[K]): T = macro StampedMacros.cachedIf[K, T]

    /** Like [[cached]], but a hit is whatever `isCurrent(stamp, value)` says. */
    def cachedWhen(isCurrent: (Int, T) => Boolean)(compute: => T)(implicit now: Stamp[K]): T =
      macro StampedMacros.cachedWhen[K, T]

    /** The value of the cache if its stamp is the current one, `orElse` otherwise; neither computes nor stores. */
    def peek(orElse: => T)(implicit now: Stamp[K]): T = macro StampedMacros.peek[K, T]
  }
}
