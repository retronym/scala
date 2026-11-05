package scala.reflect.internal.sharing.macros

import scala.reflect.macros.blackbox

/** The macros of `Sharing`: in batch mode each expansion is the code it wraps, or a call of a local method holding it.
 */
class SharingMacros(val c: blackbox.Context) extends Common {
  import c.universe._

  def unguarded[K: c.WeakTypeTag](why: Tree): Tree = evidence(HoldsType(weakTypeOf[K]))
  def unlocked[K: c.WeakTypeTag](why: Tree): Tree = evidence(MonitorType(weakTypeOf[K]))

  // ---- claims

  private def claimed(key: Tree, kind: Tree, cycleSubject: Tree, op: Tree, resultType: Type, mode: Tree,
      registry: Tree, wrap: Tree => Tree = identity): Tree = {
    val (defs, call) = localOp(op, resultType, "op", wrap)
    val ev = paramType(op)
    q"""{
      ..$defs
      if ($mode.concurrent) $registry.claim($key, $kind, $cycleSubject, () => ${call(evidence(ev))})
      else ${call(evidence(ev))}
    }"""
  }

  private def subject(key: Tree, cycleSubject: Tree): Tree =
    if (isNullLiteral(cycleSubject)) key.duplicate
    else {
      val cs = TermName(c.freshName("cycleSubject"))
      q"{ val $cs: _root_.scala.AnyRef = $cycleSubject; if ($cs == null) ${key.duplicate} else $cs }"
    }

  def claim[K: c.WeakTypeTag, T: c.WeakTypeTag](key: Tree)(op: Tree)(kind: Tree, mode: Tree, registry: Tree): Tree =
    claimed(key, kind, key.duplicate, op, weakTypeOf[T], mode, registry)

  def claimReporting[K: c.WeakTypeTag, T: c.WeakTypeTag](key: Tree, cycleSubject: Tree)(op: Tree)(
      kind: Tree, mode: Tree, registry: Tree): Tree =
    claimed(key, kind, subject(key, cycleSubject), op, weakTypeOf[T], mode, registry)

  def claimAt[K: c.WeakTypeTag, T: c.WeakTypeTag](owner: Tree)(key: Tree, kind: Tree, cycleSubject: Tree)(op: Tree)(
      mode: Tree, registry: Tree): Tree =
    claimed(key, kind, cycleSubject, op, weakTypeOf[T], mode, registry)

  private def onceImpl(key: Tree, cycleSubject: Tree, peek: Tree, compute: Tree, resultType: Type, kind: Tree,
      mode: Tree, registry: Tree): Tree = {
    val (peekDefs, peekAgain) = reusable(peek, resultType, "peek")
    val v1 = TermName(c.freshName("v"))
    val claim = claimed(key, kind, cycleSubject, compute, resultType, mode, registry,
      body => q"{ val $v1 = ${peekAgain()}; if ($v1 ne null) $v1 else $body }")
    val v = TermName(c.freshName("v"))
    q"""{
      ..$peekDefs
      val $v = ${peekAgain()}
      if ($v ne null) $v else $claim
    }"""
  }

  def once[K: c.WeakTypeTag, T: c.WeakTypeTag](key: Tree)(peek: Tree)(compute: Tree)(
      kind: Tree, mode: Tree, registry: Tree): Tree =
    onceImpl(key, key.duplicate, peek, compute, weakTypeOf[T], kind, mode, registry)

  def onceReporting[K: c.WeakTypeTag, T: c.WeakTypeTag](key: Tree, cycleSubject: Tree)(peek: Tree)(compute: Tree)(
      kind: Tree, mode: Tree, registry: Tree): Tree =
    onceImpl(key, subject(key, cycleSubject), peek, compute, weakTypeOf[T], kind, mode, registry)

  // ---- BlackHole

  def inside[T: c.WeakTypeTag](op: Tree)(h: Tree): Tree = {
    val was = TermName(c.freshName("was"))
    q"""{
      val $was = ${c.prefix.tree.duplicate}.blackHoleEnter()
      try $op finally ${c.prefix.tree.duplicate}.blackHoleLeave($was)
    }"""
  }

  def computeInside[T: c.WeakTypeTag](onCycle: Tree)(op: Tree)(h: Tree): Tree =
    q"if (${c.prefix.tree.duplicate}.isInside($h)) $onCycle else ${inside[T](op)(h)}"

  // ---- monitors

  private def monitored(lock: Tree, op: Tree, resultType: Type, mode: Tree): Tree = {
    val (defs, call) = localOp(op, resultType, "op")
    val ev = paramType(op)
    q"""{
      ..$defs
      if ($mode.concurrent) $lock.synchronized(${call(evidence(ev))}) else ${call(evidence(ev))}
    }"""
  }

  def serialised[K: c.WeakTypeTag, T: c.WeakTypeTag](lock: Tree)(op: Tree)(mode: Tree): Tree =
    monitored(lock, op, weakTypeOf[T], mode)

  def locking[K: c.WeakTypeTag, T: c.WeakTypeTag](lock: Tree)(op: Tree): Tree =
    q"$lock.synchronized(${inlineApply(op, List(evidence(paramType(op))))})"

  def locked[T: c.WeakTypeTag](op: Tree)(mode: Tree): Tree = monitored(c.prefix.tree, op, weakTypeOf[T], mode)

  def alwaysLocked[T: c.WeakTypeTag](op: Tree): Tree = locking[Nothing, T](c.prefix.tree)(op)
}
