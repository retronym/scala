package scala.reflect.internal.ijmodel

/**
 * Mirrors psi/types/recursiveUpdate/ThisTypeSubstitution.scala: the leaf update
 * that re-anchors `C.this` onto a concrete prefix (`target` = fromType,
 * `seenFromClass` = the anchor class; None is the 1-arg/null-seenFromClass form).
 *
 * Two walks, as in production:
 *   - doUpdateThisTypeFromClass: the ANCHORED lockstep climb —
 *     (target baseType clazz).prefix, clazz := clazz.containingClass — with the
 *     terminal cases (no anchor / anchor == the leaf's class / anchor top-level)
 *     falling into the narrowing walk, and the "clazz not a base of target ->
 *     narrow against pre" fallthrough.
 *   - doUpdateThisType: the NARROWING climb — isMoreNarrow, else climb
 *     containingClassType.  `escaped` records crossing a ThisType -> enclosing-
 *     class hop (leaving the target's own spine): a match after escape is
 *     scalac's UNMATCHED case and must not consume.
 *
 * Guard semantics per EngineMode:
 *   - hasRecursiveThisType0: the production pre-scan (exact containment OR
 *     isSameOrInheritor(rewritten, contained) — note the DIRECTION: the
 *     rewritten class inherits the contained this's class, the INVERSE of what
 *     the cross-symbol pump exploits, which is why production misses it).
 *   - progressBlocked: the candidate postcondition — the returned type's spine
 *     root is a this-type whose class is the same as or an inheritor of the
 *     class being rewritten (no progress => self-embedding => recirculation
 *     fuel).  Bare this-type returns are always admitted: leaf→leaf narrowing
 *     is the legitimate cake re-anchor and carries nothing to recirculate.
 */
trait ThisTypeSubstitutions { self: IjTypeSystem =>
  import symbolTable._

  case class ThisTypeSubstitution(target: Type, seenFromClass: Option[Symbol]) extends Update {
    override def toString: String =
      s"this->$target${seenFromClass.fold(" sfc=<null>")(a => s" sfc=${a.nameString}")}"

    /** The subst PF: None = blocked or unmatched (the leaf is kept and the rest
     *  of the fused chain falls through — LeafSubstitution.applyOrElse identity);
     *  Some((result, consumes)) = fired. */
    def subst(thisSym: Symbol, consumedClasses: Set[Symbol])
             (implicit mode: EngineMode): Option[(Type, Boolean)] = {
      if (mode.consumed && consumedClasses(thisSym)) return None
      if (mode.prodGuard && hasRecursiveThisType0(target, thisSym)) return None
      val walk = seenFromClass match {
        case Some(clazz) => doUpdateThisTypeFromClass(target, clazz, thisSym)
        case None        => doUpdateThisType(target, thisSym, escaped = false)
      }
      walk match {
        case Matched(res, consumes) =>
          // (the leaf-exemption inside progressBlocked also covers identity
          // returns: a bare this-type output is never blocked)
          if (mode.progress && progressBlocked(res, thisSym)) None
          else Some((res, consumes))
        case Unmatched => None
      }
    }

    private sealed trait WalkResult
    private case class Matched(res: Type, consumes: Boolean) extends WalkResult
    private case object Unmatched extends WalkResult

    /** The anchored lockstep climb. */
    private def doUpdateThisTypeFromClass(target: Type, clazz: Symbol, thisSym: Symbol): WalkResult =
      if (clazz == NoSymbol || clazz == thisSym || !clazz.owner.isClass)
        doUpdateThisType(target, thisSym, escaped = false)
      else {
        // ALLOWED-WITH-CAVEAT: scalac baseType is a CACHED lookup; IJ's
        // BaseTypes.baseType is a live recompute that re-enters asSeenFrom —
        // the spelling-doubling channel this model cannot reproduce.
        val bt = target.baseType(clazz)
        if (bt == NoType) doUpdateThisType(target, thisSym, escaped = false) // "not a base -> narrow against pre"
        else doUpdateThisTypeFromClass(bt.prefix, clazz.owner, thisSym)
      }

    /** The narrowing climb. */
    private def doUpdateThisType(target: Type, thisSym: Symbol, escaped: Boolean): WalkResult =
      if (isMoreNarrow(target, thisSym)) Matched(target, consumes = !escaped)
      else containingClassType(target) match {
        case Some(ctx) => doUpdateThisType(ctx, thisSym, escaped || target.isInstanceOf[ThisType])
        case None      => Unmatched
      }

    /** isMoreNarrow core: the target's class is the same as or an inheritor of
     *  the this-leaf's class — via bases OR self-type (IJ's ScTypeDefinition
     *  branch consults selfType), abstract types widened to their upper bound. */
    private def isMoreNarrow(target: Type, thisSym: Symbol): Boolean = {
      val cls = extractClass(target)
      cls == thisSym || cls.isSubClass(thisSym) ||
        (cls.isClass && (cls.typeOfThis.typeSymbol ne cls) && cls.typeOfThis.baseClasses.contains(thisSym))
    }

    /** containingClassType: projection prefixes stay on the spine; a this-type
     *  climbs OUT to the enclosing class's this (the escape hop). */
    private def containingClassType(tp: Type): Option[Type] = tp match {
      case ThisType(sym) =>
        val encl = sym.owner
        if (encl.isClass && !encl.isPackageClass) Some(ThisType(encl)) else None
      case SingleType(pre, _) if pre ne NoPrefix => Some(pre)
      case TypeRef(pre, _, _) if (pre ne NoPrefix) && (pre ne NoType) => Some(pre)
      case _ => None
    }

    /** The production guard (the incumbent). */
    private def hasRecursiveThisType0(tp: Type, leafSym: Symbol): Boolean =
      tp.exists {
        case ThisType(s) => s == leafSym || leafSym.isSubClass(s)
        case _           => false
      }

    /** The Progress postcondition (the candidate), with the leaf-exemption. */
    private def progressBlocked(res: Type, leafSym: Symbol): Boolean = res match {
      case _: ThisType => false
      case _ =>
        spineRootThis(res) match {
          case Some(root) => root == leafSym || root.isSubClass(leafSym)
          case None       => false
        }
    }

    private def spineRootThis(t: Type): Option[Symbol] = t match {
      case ThisType(s) => Some(s)
      case SingleType(pre, _) => spineRootThis(pre)
      case TypeRef(pre, _, _) if (pre ne NoPrefix) && (pre ne NoType) => spineRootThis(pre)
      case _ => None
    }
  }
}
