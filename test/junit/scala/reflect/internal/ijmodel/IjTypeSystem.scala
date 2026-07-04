package scala.reflect.internal.ijmodel

import scala.reflect.internal.SymbolTable

/**
 * A replica of IntelliJ-Scala's member lookup WITH substitution, built over scalac
 * Types/Symbols, so its limitations and scalac-parity can be reasoned about in one
 * place with scalac's own `memberType` as the asserted oracle.
 *
 * Each file and construct mirrors its intellij-scala counterpart by name
 * (all under `scala/scala-impl/src/org/jetbrains/plugins/scala/lang/`):
 *
 *   file / construct here                    mirrors
 *   ─────────────────────────────────────────────────────────────────────────
 *   Update.scala          `Update`           psi/types/recursiveUpdate/Update.scala
 *   ScSubstitutor.scala   `ScSubstitutor`    psi/types/recursiveUpdate/ScSubstitutor.scala
 *                         `recursiveUpdateImpl`, `followed`, `followUpdateThisType`,
 *                         `TypeParamSubstitution`, `ScSubstitutor.declarationAnchor`
 *   ThisTypeSubstitution.scala
 *                         `ThisTypeSubstitution(target, seenFromClass)`
 *                                            psi/types/recursiveUpdate/ThisTypeSubstitution.scala
 *                         `doUpdateThisTypeFromClass` (anchored lockstep climb),
 *                         `doUpdateThisType` (narrowing climb), `isMoreNarrow`,
 *                         `containingClassType`, `hasRecursiveThisType0`
 *   MixinNodes.scala      `TypeDefinitionMembers.getSignatures`, `Signature`,
 *                         `MixinNodes.SuperTypesData`
 *                                            psi/impl/toplevel/typedef/MixinNodes.scala
 *   ScalaResolveState.scala
 *                         `ScalaResolveState` (substitutor + compoundOrThisType)
 *                                            resolve/ScalaResolveState.scala
 *   ScProjectionType.scala
 *                         `ScProjectionType.actualSubst`
 *                                            psi/types/api/designator/ScProjectionType.scala
 *   BaseProcessor.scala   `BaseProcessor` (`processType`/`processTypeImpl`/
 *                         `processElement`/`execute`, `RecursionState`)
 *                                            resolve/processor/BaseProcessor.scala
 *   ReferenceExpressionResolver.scala
 *                         `ReferenceExpressionResolver` (reference-level resolution
 *                         with the `substitutorWithThisType` prepend)
 *                                            resolve/ReferenceExpressionResolver.scala
 *   EngineMode.scala      research knob (no IJ analog): Production incumbent vs
 *                         Progress+Consumed candidate vs Unguarded
 *
 * ══ OUT OF BOUNDS ══
 * The point of the replica is to NOT delegate to the scalac machinery it models.
 * Forbidden in model code (allowed ONLY in test oracles and fixture/golden
 * construction):
 *   - Type#member / members / findMember / nonPrivateMember   (member lookup —
 *     scalac's own linearization walk with asSeenFrom baked in)
 *   - Type#memberType / memberInfo                            (the keystone)
 *   - Type#asSeenFrom / AsSeenFromMap                         (the map itself)
 *   - Type#subst / substSym / substThis                       (scalac substitution)
 *   - Type#baseTypeSeq                                        (the cached BTS)
 * Allowed, as declared IntelliJ analogs:
 *   - Symbol#info.decls / info.parents      (PSI declarations / extends clauses)
 *   - member.info                           (the member's DECLARED type — PSI
 *     `e.type()`; never asked through a prefix)
 *   - Type#baseType(clazz)                  (IJ BaseTypes.baseType — CAVEAT: scalac's
 *     is a CACHED, inert lookup while IJ's is a live recompute that re-enters
 *     asSeenFrom; this model therefore CANNOT reproduce the re-entrant
 *     spelling-doubling channel, only the recirculation channel)
 *   - Symbol#typeOfThis                     (PSI selfType)
 *   - Symbol#isSubClass                     (isInheritorDeep)
 *   - Type#widen / dealias / bounds.hi      (path underlying / alias expansion /
 *     abstract-type upper bound)
 *   - Type#prefix on TypeRef/SingleType     (plain accessors) — but NOT on
 *     ThisType: ThisType.prefix delegates to underlying.prefix, and computing a
 *     PACKAGE's underlying runs owner.thisType.memberType(pkg).  The model stays
 *     clear by never treating package this-types as substitution candidates and
 *     terminating the anchored climb at the outermost class (PSI packages are
 *     not classes) — a runtime trace caught this leaking (see IjModelTraceTest)
 *   - Type#exists                           (subtypeExists)
 *   - <:< in the self-type dispatch         (stand-in for IJ's OWN conforms();
 *     IJ conformance is a separate subsystem not modeled here)
 *   - singleType / ThisType / TypeRef ctors (type construction, not lookup)
 */
trait IjTypeSystem
  extends Updates
  with ScSubstitutors
  with ThisTypeSubstitutions
  with MixinNodes
  with ScalaResolveStates
  with ScProjectionTypes
  with BaseProcessors
  with ReferenceExpressionResolvers {

  val symbolTable: SymbolTable
  import symbolTable._

  /** ScType#extractClass analog: widen paths to their underlying, abstract types /
   *  type params to their upper bound, and return the class symbol. */
  def extractClass(t: Type): Symbol = {
    val w = t.widen.dealias
    val s = w.typeSymbol
    if (s.isClass) s
    else if (w.bounds.hi ne w) extractClass(w.bounds.hi)
    else s
  }
}
