package scala.reflect.internal.ijmodel

/**
 * Mirrors psi/types/recursiveUpdate/ScSubstitutor.scala: the fused substitution
 * chain.  `followed` is blind concatenation (the duplication channel);
 * `followUpdateThisType` is the resolve-state prepend; `recursiveUpdateImpl` is
 * the engine — the first matching update REPLACES a leaf and the REMAINDER of
 * the chain processes the replacement; non-matching (or blocked) updates fall
 * through to the next update (LeafSubstitution.applyOrElse identity); nodes no
 * update matches descend with the full remaining chain.
 */
trait ScSubstitutors { self: IjTypeSystem =>
  import symbolTable._

  /** TypeParamSubstitution (held in ScSubstitutor.scala in IJ too). */
  case class TypeParamSubstitution(tvMap: Map[Symbol, Type]) extends Update {
    override def toString: String =
      tvMap.map { case (k, v) => s"${k.nameString}:=$v" }.mkString("[", ",", "]")
  }

  class ScSubstitutor(val substitutions: Vector[Update]) {
    def isEmpty: Boolean = substitutions.isEmpty

    def followed(other: ScSubstitutor): ScSubstitutor =
      if (isEmpty) other
      else if (other.isEmpty) this
      else new ScSubstitutor(substitutions ++ other.substitutions)

    /** ScalaResolveState.substitutorWithThisType: PREPEND a fresh this-subst. */
    def followUpdateThisType(fromType: Type, seenFromClass: Symbol): ScSubstitutor =
      new ScSubstitutor(ThisTypeSubstitution(fromType, Option(seenFromClass)) +: substitutions)

    def render: String = substitutions.mkString("  |  ")
    def thisSubstCount: Int = substitutions.count(_.isInstanceOf[ThisTypeSubstitution])
    def duplicateThisTargets: Boolean = {
      val ts = substitutions.collect { case ThisTypeSubstitution(t, _) => t.toString }
      ts.distinct.length < ts.length
    }

    /** Dedup by update equality (targets are hash-consed, so case-class equality
     *  is cheap).  THEOREM (under fall-through + remainder-only + consumed): a
     *  repeated identical update is inert — at any lineage point its class is
     *  either consumed (skip) or the first occurrence already failed to match
     *  (identity, fall through) — so dedup is pure perf. */
    def dedupped: ScSubstitutor = new ScSubstitutor(substitutions.distinct)

    def apply(tp: Type)(implicit mode: EngineMode): Type =
      recursiveUpdateImpl(tp, 0, Set.empty)

    /** The fused engine.  Semantics: PER-LINEAGE sequential composition — each
     *  original leaf's fate is a pure function of (leaf, chain); the consumed
     *  set threads into the replacement lineage only. */
    private def recursiveUpdateImpl(tp: Type, from: Int, consumedClasses: Set[Symbol])
                                   (implicit mode: EngineMode): Type = {
      var i = from
      while (i < substitutions.length) {
        substitutions(i) match {
          case tts: ThisTypeSubstitution =>
            tp match {
              case ThisType(sym) =>
                tts.subst(sym, consumedClasses) match {
                  case Some((res, consumes)) =>
                    val consumed1 =
                      if (mode.consumed && consumes) consumedClasses + sym else consumedClasses
                    return recursiveUpdateImpl(res, i + 1, consumed1)
                  case None =>
                    i += 1 // blocked/unmatched: keep the leaf, fall through to the rest
                }
              case _ => i += 1
            }
          case TypeParamSubstitution(tvMap) =>
            tp match {
              case TypeRef(_, sym, _) if tvMap.contains(sym) =>
                return recursiveUpdateImpl(tvMap(sym), i + 1, consumedClasses)
              case _ => i += 1
            }
          case _ => i += 1
        }
      }
      // no update matched this node: descend with the full remaining chain
      val descend = new TypeMap {
        def apply(t: Type): Type = recursiveUpdateImpl(t, from, consumedClasses)
      }
      descend.mapOver(tp)
    }
  }

  object ScSubstitutor {
    val empty: ScSubstitutor = new ScSubstitutor(Vector.empty)

    /** The 2-arg this-substitutor (anchored). */
    def apply(target: Type, seenFromClass: Symbol): ScSubstitutor =
      new ScSubstitutor(Vector(ThisTypeSubstitution(target, Some(seenFromClass))))

    /** The 1-arg/null-seenFromClass form (the anchorless minting sites). */
    def apply(target: Type): ScSubstitutor =
      new ScSubstitutor(Vector(ThisTypeSubstitution(target, None)))

    /** ScSubstitutor.declarationAnchor: the member's containing class. */
    def declarationAnchor(member: Symbol): Symbol =
      if (member.owner.isClass) member.owner else NoSymbol
  }
}
