package io.github.retronym.sharing


/** The companion of [[Once]], an abstract type in the Scala 2.13 build: see the package object. A field, its handle
 *  and its claim kind:
 *
 *  {{{
 *  class Symbol(name: String)(implicit mode: Mode, registry: ClaimRegistry) {
 *    private[mypkg] var myInfo: Once[this.type, Info] = Once.empty
 *    def info: Info = myInfo.get(_ => computeInfo())
 *  }
 *  object Symbol {
 *    implicit val kind: Sharing.ClaimKind[Symbol] = Sharing.ClaimKind(new What("computing info", null), lockWhenSerialised = true)
 *    implicit val myInfoHandle: Published.Handle[Symbol, "myInfo"] = Published.handle
 *  }
 *  }}}
 */
object Once {
  /** The field's initial value: no fact yet. */
  def empty[K, T <: AnyRef]: Once[K, T] = null.asInstanceOf[Once[K, T]]
}
