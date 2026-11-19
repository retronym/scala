package scala.reflect.internal.sharing.macros

import scala.reflect.macros.blackbox

/** The macros of `Stamped`: a read of the field and a compare of the stamp, and on a miss the computation and a store,
 *  in every mode. */
class StampedMacros(val c: blackbox.Context) extends Common {
  import c.universe._

  /** The cache's field: a `var` of a class, or a local `var`. */
  private def cacheVar: Tree = {
    val t = strip(prefixArg)
    val ok = t match {
      case s: Select => s.symbol != null && s.symbol.isMethod && s.symbol.asMethod.isGetter && s.symbol.asMethod.setter != NoSymbol
      case i: Ident => i.symbol != null && i.symbol.isTerm && i.symbol.asTerm.isVar
      case _ => false
    }
    if (!ok) c.abort(t.pos, s"a cache in a `Stamped` must be a `var`, not $t")
    t
  }

  /** The expansion of `cached`, `cachedIf` and `cachedWhen`: `isCurrent(n, stamp, value)`, given the current stamp
   *  `n`, says whether the stored value is a hit; `keep(v)`, if any, whether to store the value computed. */
  private def cachedAt[K: c.WeakTypeTag, T: c.WeakTypeTag](now: Tree, compute: Tree,
      isCurrent: (Tree, Tree, Tree) => Tree, keep: Option[Tree => Tree]): Tree = {
    val (k, t) = (weakTypeOf[K], weakTypeOf[T])
    val field = cacheVar
    val (n, cell, v) = (TermName(c.freshName("n")), TermName(c.freshName("c")), TermName(c.freshName("v")))
    val store = q"${field.duplicate} = new $pkg.Stamped[$k, $t]($n, $v)"
    q"""{
      val $n: _root_.scala.Int = $now.value
      val $cell = ${field.duplicate}
      if (${isCurrent(q"$n", q"$cell.stamp", q"$cell.value")}) $cell.value
      else {
        val $v: $t = $compute
        ${keep.fold(store)(k => q"if (${k(q"$v")}) $store")}
        $v
      }
    }"""
  }

  def cached[K: c.WeakTypeTag, T: c.WeakTypeTag](compute: Tree)(now: Tree): Tree =
    cachedAt[K, T](now, compute, (n, s, _) => q"$s == $n", None)

  def cachedIf[K: c.WeakTypeTag, T: c.WeakTypeTag](keep: Tree)(compute: Tree)(now: Tree): Tree =
    cachedAt[K, T](now, compute, (n, s, _) => q"$s == $n", Some(v => inlineApply(keep, List(v))))

  def cachedWhen[K: c.WeakTypeTag, T: c.WeakTypeTag](isCurrent: Tree)(compute: Tree)(now: Tree): Tree =
    cachedAt[K, T](now, compute, (_, s, v) => inlineApply(isCurrent, List(s, v)), None)

  def peek[K: c.WeakTypeTag, T: c.WeakTypeTag](orElse: Tree)(now: Tree): Tree = {
    val cell = TermName(c.freshName("c"))
    q"{ val $cell = ${cacheVar.duplicate}; if ($cell.stamp == $now.value) $cell.value else $orElse }"
  }
}
