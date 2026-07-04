package scala.reflect.internal.ijmodel

/**
 * Mirrors resolve/processor/BaseProcessor.scala: the type dispatch
 * (`processTypeImpl`) and element processing (`processElement`) that together
 * perform member lookup, threading `ScalaResolveState` and minting substitutors
 * at the projection and self-type sites.
 *
 *   - ThisType: the three self-type branches (no self / self-conforms ->
 *     recurse into the SELF TYPE with the substitutor REPLACED — not followed —
 *     by ScSubstitutor(ThisType(clazz), clazz) and compoundOrThisType set /
 *     reverse-conforms -> processElement / else glb, approximated as both parts).
 *   - SingleType (ScProjectionType over a term path): mints
 *     `ScSubstitutor(proj, declarationAnchor(elem)) followed actualSubst` — the
 *     BaseProcessor.processTypeImpl:315 site from the production traces.
 *   - abstract type / type param -> upper bound with
 *     updateWithProjectionSubst=false and the visitedTypeParameter breaker.
 *   - TypeRef over a class (ScProjectionType/ParameterizedType): designator args
 *     as TypeParamSubstitution + signature substitutor + projection this-subst.
 *   - RefinedType -> the refinement class's signatures (compound
 *     processDeclarations).
 *
 * `processElement`'s typed-definition branch is the EAGER substitute-then-
 * recurse (`processTypeImpl(newSubst(declaredType), ...)`) — the synchronous
 * recirculation channel: a spelling grown by the substitution immediately
 * becomes the next dispatch's input type.
 *
 * `RecursionState` (the "ugly recursion breakers") bounds growth WITHIN a pass;
 * growth ACROSS passes — fresh processor per reference resolution — is
 * unbounded, as in production.
 */
trait BaseProcessors { self: IjTypeSystem =>
  import symbolTable._

  object BaseProcessor {
    case class RecursionState(visitedProjections: Set[Symbol], visitedTypeParameter: Set[Symbol]) {
      def add(projection: Symbol): RecursionState =
        copy(visitedProjections = visitedProjections + projection)
      def addTypeParameter(tp: Symbol): RecursionState =
        copy(visitedTypeParameter = visitedTypeParameter + tp)
    }
    object RecursionState {
      val empty: RecursionState = RecursionState(Set.empty, Set.empty)
    }
  }
  import BaseProcessor._

  /** The processor: collects (namedElement, substitutor) candidates by name. */
  class BaseProcessor(name: Name)(implicit mode: EngineMode) {
    var candidatesSet: List[(Symbol, ScSubstitutor)] = Nil

    def execute(namedElement: Symbol, state: ScalaResolveState): Boolean = {
      if (namedElement.name == name && !namedElement.isConstructor)
        candidatesSet ::= (namedElement, state.substitutor)
      true
    }

    def processType(t: Type): Unit =
      processTypeImpl(t, ScalaResolveState.empty)(RecursionState.empty)

    def processTypeImpl(t: Type, state: ScalaResolveState, updateWithProjectionSubst: Boolean = true)
                       (implicit recState: RecursionState): Boolean = t match {

      // ScThisType(clazz): the self-type branches
      case ThisType(clazz) =>
        val selfTp = clazz.typeOfThis
        val hasSelf = selfTp.typeSymbol ne clazz
        if (!hasSelf)
          processElement(clazz, ScSubstitutor.empty, state)
        // <:< below stands in for IJ's own conforms() (a separate subsystem)
        else if (selfTp <:< clazz.tpe_*) {
          // self conforms: recurse into the SELF TYPE, substitutor REPLACED (not
          // followed) by ScSubstitutor(ScThisType(clazz), clazz), compound flag set
          val newState = state.withCompoundOrSelfType
            .withSubstitutor(ScSubstitutor(ThisType(clazz), clazz))
          processTypeImpl(selfTp, newState)
        }
        else if (clazz.tpe_* <:< selfTp)
          processElement(clazz, ScSubstitutor.empty, state)
        else {
          // glb(self, clazz): approximate with intersection — process both parts
          val newState = state.withCompoundOrSelfType
          processTypeImpl(selfTp, newState) && processTypeImpl(clazz.tpe_*, newState)
        }

      // ScProjectionType over a term path (val/object member): mints
      // ScSubstitutor(proj, declarationAnchor(elem)) followed actualSubst
      case st @ SingleType(pre, elem) =>
        if (recState.visitedProjections.contains(elem)) true
        else {
          val actualSubst = ScProjectionType.actualSubst(pre, elem)
          val s =
            if (updateWithProjectionSubst)
              actualSubst.followUpdateThisType(st, ScSubstitutor.declarationAnchor(elem))
            else actualSubst
          processElement(elem, s, state)(recState.add(elem))
        }

      // TypeParameterType: recurse the upper bound, projection substs off
      case tr @ TypeRef(_, sym, _) if !sym.isClass && (tr.bounds.hi ne tr) =>
        if (recState.visitedTypeParameter.contains(sym)) true
        else processTypeImpl(tr.bounds.hi, state, updateWithProjectionSubst = false)(recState.addTypeParameter(sym))

      // ScProjectionType / ParameterizedType over a class designator
      case tr @ TypeRef(pre, cls, args) if cls.isClass =>
        if (recState.visitedProjections.contains(cls)) true
        else {
          val designatorArgs =
            if (args.nonEmpty)
              new ScSubstitutor(Vector(TypeParamSubstitution(cls.typeParams.zip(args).toMap)))
            else ScSubstitutor.empty
          val actualSubst = ScProjectionType.actualSubst(pre, cls).followed(designatorArgs)
          val s =
            if (updateWithProjectionSubst && (pre ne NoPrefix) && (pre ne NoType))
              actualSubst.followUpdateThisType(tr, ScSubstitutor.declarationAnchor(cls))
            else actualSubst
          processElement(cls, s, state)(recState.add(cls))
        }

      // ScCompoundType: refinement class carries decls + parents via signatures
      case rt: RefinedType =>
        processElement(rt.typeSymbol, ScSubstitutor.empty, state)

      case NullaryMethodType(res) => processTypeImpl(res, state, updateWithProjectionSubst)

      case _ => true
    }

    private def processElement(e: Symbol, s: ScSubstitutor, state: ScalaResolveState)
                              (implicit recState: RecursionState): Boolean = {
      // "val newSubst = if (compoundOrThis.nonEmpty) subst else subst.followed(s)"
      val newSubst = if (state.compoundOrThisType) state.substitutor else state.substitutor.followed(s)
      val stateWithSubst = state.withSubstitutor(newSubst).copy(compoundOrThisType = false)

      if (e.isClass) {
        // processClassDeclarations: execute every MixinNodes signature with
        // sig.substitutor composed under the accumulated state substitutor
        TypeDefinitionMembers.getSignatures(e).forall { sig =>
          execute(sig.namedElement, stateWithSubst.withSubstitutor(sig.substitutor.followed(newSubst)))
        }
      }
      else if (e.isTerm && e.isModule)
        processElement(e.moduleClass, s, state)
      else if (e.isTerm) {
        // ScTypedDefinition: EAGERLY substitute the declared type, then recurse —
        // the synchronous recirculation channel (a spelling grown by newSubst
        // immediately becomes the next dispatch's input type)
        val declared = e.info.resultType
        processTypeImpl(newSubst(declared), stateWithSubst, updateWithProjectionSubst = false)
      }
      else true
    }
  }
}
