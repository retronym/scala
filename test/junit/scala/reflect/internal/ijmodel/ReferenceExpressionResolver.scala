package scala.reflect.internal.ijmodel

/**
 * Mirrors resolve/ReferenceExpressionResolver.scala: reference-level resolution.
 * Each reference gets a FRESH processor (as in production), and the candidate's
 * substitutor receives the ScalaResolveState.substitutorWithThisType prepend
 * (fromType = the qualifier prefix, seenFromClass = the member's declaring
 * class).  Recirculation across a qualified-reference chain happens through the
 * computed TYPES (paths for stable members, computed spellings otherwise) —
 * the across-passes growth channel.
 */
trait ReferenceExpressionResolvers { self: IjTypeSystem =>
  import symbolTable._

  object ReferenceExpressionResolver {

    /** Resolve `name` against `pre`.  Returns the resolved SYMBOL too, so
     *  callers never need scalac's own `Type#member` (out of bounds) to
     *  continue a path. */
    def resolve(pre: Type, name: String)(implicit mode: EngineMode): (Symbol, Type, ScSubstitutor) = {
      val processor = new BaseProcessor(TermName(name))
      processor.processType(pre)
      val (m, subst) = processor.candidatesSet.headOption
        .getOrElse(sys.error(s"member $name not found on $pre"))
      val full = subst.followUpdateThisType(pre, m.owner)
      (m, full(m.info).resultType, full)
    }

    def memberType(pre: Type, name: String)(implicit mode: EngineMode): (Type, ScSubstitutor) = {
      val (_, tp, chain) = resolve(pre, name)
      (tp, chain)
    }

    /** Qualified-reference chain: each step a fresh processor run. */
    def resolvePath(root: Type, names: String*)(implicit mode: EngineMode): (Type, ScSubstitutor) = {
      var pre: Type = root
      var lastType: Type = root
      var lastChain: ScSubstitutor = ScSubstitutor.empty
      for (name <- names) {
        val (sym, computed, chain) = resolve(pre, name)
        lastType = computed
        lastChain = chain
        pre = if (sym.isStable) singleType(pre, sym) else computed
      }
      (lastType, lastChain)
    }
  }
}
