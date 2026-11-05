package io.github.retronym.sharing.macros

import scala.reflect.macros.blackbox

/** The macros of `Once`: in batch mode a plain read and a null test on a hit, a plain store on a miss. */
class OnceMacros(val c: blackbox.Context) extends Common {
  import c.universe._

  /** The field, whose owner must be the `K` of its type: the claim is taken on it, and `compute` given `Holds[K]`. */
  private def onceField(k: Type): Field = {
    val f = classVar(prefixArg, "a `Once` operation")
    if (!(f.qual.tpe =:= k))
      c.abort(f.tree.pos, s"${f.tree} is a `Once` keyed by $k, not by its owner ${f.qual.tpe}: declare it `Once[this.type, ...]`")
    f
  }

  private def acquireRaw(f: Field, t: Type): Tree = q"$publish.getAcquireRef(${handleFor(f)}, ${f.owner}).asInstanceOf[$t]"
  private def releaseRaw(f: Field, v: Tree): Tree = q"$publish.setReleaseRef(${handleFor(f)}, ${f.owner}, $v)"
  private def plainRaw(f: Field, t: Type): Tree = q"${f.read}.asInstanceOf[$t]"
  private def storeRaw(f: Field, k: Type, t: Type, v: Tree): Tree = f.store(q"$v.asInstanceOf[${OnceType(k, t)}]")

  def get[K: c.WeakTypeTag, T: c.WeakTypeTag](compute: Tree)(kind: Tree, mode: Tree, registry: Tree): Tree = {
    val (k, t) = (weakTypeOf[K], weakTypeOf[T])
    val f = onceField(k)
    // `compute` is run on a miss in either mode: a local method, called from both
    val (defs, call) = localOp(compute, t, "compute")
    val ev = paramType(compute)
    val (v, v1, r) = (TermName(c.freshName("v")), TermName(c.freshName("v")), TermName(c.freshName("r")))
    q"""{
      ..$defs
      if ($mode.concurrent) {
        val $v = ${acquireRaw(f, t)}
        if ($v ne null) $v
        else $registry.claim(${f.owner}, $kind, ${f.owner}, () => {
          val $v1 = ${plainRaw(f, t)}
          if ($v1 ne null) $v1
          else {
            val $r = ${call(evidence(ev))}
            ${releaseRaw(f, q"$r")}
            $r
          }
        })
      } else {
        val $v = ${plainRaw(f, t)}
        if ($v ne null) $v
        else {
          val $r = ${call(evidence(ev))}
          ${storeRaw(f, k, t, q"$r")}
          $r
        }
      }
    }"""
  }

  def peek[T: c.WeakTypeTag](mode: Tree): Tree = {
    val f = classVar(prefixArg, "a `Once` operation")
    q"if ($mode.concurrent) ${acquireRaw(f, weakTypeOf[T])} else ${plainRaw(f, weakTypeOf[T])}"
  }

  def acquire[T: c.WeakTypeTag]: Tree = acquireRaw(classVar(prefixArg, "a `Once` operation"), weakTypeOf[T])

  private def storing[K: c.WeakTypeTag, T: c.WeakTypeTag](v: Tree, mode: Tree): Tree = {
    val (k, t) = (weakTypeOf[K], weakTypeOf[T])
    val f = classVar(prefixArg, "a `Once` operation")
    val x = TermName(c.freshName("v"))
    q"{ val $x: $t = $v; if ($mode.concurrent) ${releaseRaw(f, q"$x")} else ${storeRaw(f, k, t, q"$x")} }"
  }

  def set[K: c.WeakTypeTag, T: c.WeakTypeTag](v: Tree)(h: Tree, mode: Tree): Tree = storing[K, T](v, mode)

  def reset[K: c.WeakTypeTag, T: c.WeakTypeTag]()(h: Tree, mode: Tree): Tree = storing[K, T](q"null", mode)

  def setPlain[K: c.WeakTypeTag, T: c.WeakTypeTag](v: Tree): Tree =
    storeRaw(classVar(prefixArg, "a `Once` operation"), weakTypeOf[K], weakTypeOf[T], v)
}
