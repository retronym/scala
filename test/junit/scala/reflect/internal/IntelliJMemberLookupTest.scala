package scala.reflect.internal

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

/**
 * A replica of IntelliJ-Scala's member lookup WITH substitution, built over scalac
 * Types/Symbols, so its limitations and scalac-parity can be reasoned about in one
 * place with scalac's own `memberType` as the asserted oracle.
 *
 * Structure mirrors the real pipeline component-for-component:
 *
 *   BaseProcessor.processTypeImpl  -> `processTypeImpl`: the type dispatch —
 *     - ScThisType: self-type branches (none / self==this / self-conforms ->
 *       recurse into self with state.withCompoundOrSelfType and substitutor
 *       REPLACED by ScSubstitutor(ScThisType(clazz), clazz) / reverse-conforms ->
 *       processElement / else glb).
 *     - ScProjectionType (paths, here SingleType/prefixed TypeRef): mints
 *       `ScSubstitutor(proj, declarationAnchor(elem)) followed actualSubst` —
 *       the BaseProcessor.processTypeImpl:315 site from the traces.  The
 *       actualSubst analog is the type-member signature's substitutor from the
 *       prefix's class (MixinNodes hops) plus the prefix designator's type args.
 *     - TypeParameterType -> upper bound with updateWithProjectionSubst=false.
 *     - ParameterizedType -> designator args as ParamUpd, then processElement.
 *     - compound/refinement -> the refinement class's signatures.
 *   BaseProcessor.processElement   -> `processElement`:
 *     - newSubst = state.substitutor.followed(s) UNLESS compoundOrThis is set
 *       (the ugly-workaround flag, mirrored).
 *     - class      -> processClassDeclarations: execute each MixinNodes signature
 *       with sig.subst.followed(newSubst).
 *     - typed def  -> processTypeImpl(newSubst(declaredType), stateWithSubst,
 *       updateWithProjectionSubst = false): the EAGER substitute-then-recurse that
 *       is the synchronous recirculation channel (a grown spelling produced by the
 *       substitution immediately becomes the next dispatch's type).
 *   RecursionState                 -> visitedProjections/visitedTypeParameter
 *     recursion breakers (bounds growth WITHIN a pass; growth ACROSS passes —
 *     fresh processor per reference resolution — is unbounded, as in production).
 *   TypeDefinitionMembers/MixinNodes -> `signatures`: inherited signatures carry
 *     substitutors composed recursively along parent hops (declaring side first,
 *     hop after, so a hop's replacement is processed by the chain remainder).
 *   ScalaResolveState.substitutorWithThisType -> the reference-level prepend in
 *     `ijMemberType` (fromType, seenFromClass = declaring class).
 *
 * The engine (`IjSubst.apply`) mirrors ScSubstitutor.recursiveUpdateImpl (first
 * matching update replaces the leaf, the REMAINDER processes the replacement) and
 * ThisTypeSubstitution (anchored lockstep baseType/owner climb, isMoreNarrow
 * narrowing with self-type awareness, escape-climb through enclosing this-types).
 *
 * ══ OUT OF BOUNDS ══
 * The point of the replica is to NOT delegate to the scalac machinery it models.
 * Forbidden in model code (allowed ONLY in the clearly-marked oracle section and
 * in fixture/golden construction):
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
 *   - Type#exists                           (subtypeExists)
 *   - <:< in the self-type dispatch         (stand-in for IJ's OWN conforms();
 *     IJ conformance is a separate subsystem not modeled here)
 *   - singleType / ThisType / TypeRef ctors (type construction, not lookup)
 *
 * Switchable semantics (EngineMode):
 *   - prodGuard: hasRecursiveThisType0 pre-scan (exact this containment OR
 *     rewrittenClass.isSubClass(containedThisClass)).
 *   - progress:  postcondition — refuse a this-rewrite whose OUTPUT is a this-rooted
 *     PATH still rooted (via inheritance) in the class being rewritten; bare
 *     this-type outputs (leaf→leaf narrowings) always admitted.
 *   - consumed:  first-match-wins per this-class within one chain application; a
 *     match reached only through the escape-climb (scalac's unmatched case) does
 *     NOT consume.
 */
@RunWith(classOf[JUnit4])
class IntelliJMemberLookupTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  // ═══════════════════════════════════════════════════════════════════════
  //  Engine (ScSubstitutor / ThisTypeSubstitution)
  // ═══════════════════════════════════════════════════════════════════════

  case class EngineMode(prodGuard: Boolean, progress: Boolean, consumed: Boolean) {
    override def toString =
      if (prodGuard) "Production"
      else if (progress && consumed) "Progress+Consumed"
      else if (progress) "Progress"
      else if (consumed) "Consumed"
      else "Unguarded"
  }
  // The three modes that matter: Unguarded shows the disease, Production is the
  // incumbent, Progress+Consumed is the candidate.  (Progress-only was dropped
  // from the matrix: it is a component, not a candidate — without consumption it
  // over-narrows redundant chains, the SCL-7008 shape.)
  val Production       = EngineMode(prodGuard = true,  progress = false, consumed = false)
  val Unguarded        = EngineMode(prodGuard = false, progress = false, consumed = false)
  val ProgressConsumed = EngineMode(prodGuard = false, progress = true,  consumed = true)
  val allModes = List(Production, Unguarded, ProgressConsumed)

  sealed trait Upd
  /** ThisTypeSubstitution(target = fromType, seenFromClass = anchor); anchor=None is
   *  the 1-arg/null-seenFromClass form (the anchorless minting sites). */
  case class ThisUpd(target: Type, anchor: Option[Symbol]) extends Upd {
    override def toString = s"this->$target${anchor.fold(" sfc=<null>")(a => s" sfc=${a.nameString}")}"
  }
  /** TypeParamSubstitution. */
  case class ParamUpd(map: Map[Symbol, Type]) extends Upd {
    override def toString = map.map { case (k, v) => s"${k.nameString}:=$v" }.mkString("[", ",", "]")
  }

  class IjSubst(val updates: Vector[Upd]) {
    def isEmpty: Boolean = updates.isEmpty
    def followed(other: IjSubst): IjSubst =
      if (isEmpty) other else if (other.isEmpty) this else new IjSubst(updates ++ other.updates)
    /** ScalaResolveState.substitutorWithThisType: PREPEND a fresh this-subst. */
    def followUpdateThisType(fromType: Type, seenFromClass: Symbol): IjSubst =
      new IjSubst(ThisUpd(fromType, Option(seenFromClass)) +: updates)

    def render: String = updates.mkString("  |  ")
    def thisSubstCount: Int = updates.count(_.isInstanceOf[ThisUpd])
    def duplicateThisTargets: Boolean = {
      val ts = updates.collect { case ThisUpd(t, _) => t.toString }
      ts.distinct.length < ts.length
    }

    def apply(tp: Type)(implicit mode: EngineMode): Type = applyFrom(tp, 0, Set.empty)

    /** Dedup by update equality (targets are hash-consed, so ThisUpd/ParamUpd
     *  case-class equality is cheap).  THEOREM (under fall-through + remainder-only
     *  + consumed): a repeated identical update is inert — at any lineage point its
     *  class is either consumed (skip) or the first occurrence already failed to
     *  match (identity, fall through) — so dedup is pure perf. */
    def dedupped: IjSubst = new IjSubst(updates.distinct)

    // recursiveUpdateImpl: try updates from index `from`; a match replaces the leaf
    // and the REMAINDER processes the replacement (with the matched this-class
    // consumed when the walk says so); non-matching nodes descend with the full
    // remaining chain.
    private def applyFrom(tp: Type, from: Int, consumedClasses: Set[Symbol])(implicit mode: EngineMode): Type = {
      var i = from
      while (i < updates.length) {
        updates(i) match {
          case ThisUpd(target, anchor) =>
            tp match {
              case ThisType(sym) if !(mode.consumed && consumedClasses(sym)) &&
                                    !(mode.prodGuard && prodGuardBlocks(target, sym)) =>
                val walk = anchor match {
                  case Some(clazz) => anchoredWalk(target, clazz, sym)
                  case None        => narrowWalk(target, sym, escaped = false)
                }
                walk match {
                  case Matched(res, consumes) =>
                    val blocked = mode.progress && (res ne tp) && progressBlocks(res, sym)
                    val (res1, consumes1) = if (blocked) (tp, false) else (res, consumes)
                    val consumed1 = if (mode.consumed && consumes1) consumedClasses + sym else consumedClasses
                    return applyFrom(res1, i + 1, consumed1)
                  case Unmatched =>
                    i += 1 // walk exhausted: keep the leaf, let later updates try
                }
              case _ => i += 1
            }
          case ParamUpd(map) =>
            tp match {
              case TypeRef(_, sym, _) if map.contains(sym) =>
                return applyFrom(map(sym), i + 1, consumedClasses)
              case _ => i += 1
            }
        }
      }
      // no update matched this node: descend with the full remaining chain
      val self = this
      val descend = new TypeMap {
        def apply(t: Type): Type = self.applyFrom(t, from, consumedClasses)
      }
      descend.mapOver(tp)
    }

    // ── the this-walk, ThisTypeSubstitution faithfully ──────────────────────
    private sealed trait WalkResult
    private case class Matched(res: Type, consumes: Boolean) extends WalkResult
    private case object Unmatched extends WalkResult

    /** doUpdateThisTypeFromClass: lockstep (target baseType clazz).prefix / clazz.owner,
     *  terminal cases fall into the narrowing walk. */
    private def anchoredWalk(target: Type, clazz: Symbol, thisSym: Symbol): WalkResult =
      if (clazz == NoSymbol || clazz == thisSym || !clazz.owner.isClass)
        narrowWalk(target, thisSym, escaped = false)
      else {
        // ALLOWED-WITH-CAVEAT: scalac baseType is a CACHED lookup; IJ's
        // BaseTypes.baseType is a live recompute that re-enters asSeenFrom —
        // the spelling-doubling channel this model cannot reproduce.
        val bt = target.baseType(clazz)
        if (bt == NoType) narrowWalk(target, thisSym, escaped = false) // "not a base -> narrow against pre"
        else anchoredWalk(bt.prefix, clazz.owner, thisSym)
      }

    /** doUpdateThisType: isMoreNarrow / containingClassType climb.  `escaped` records
     *  crossing a ThisType -> enclosing-class hop (leaving the target's own spine):
     *  a match after escape is scalac's UNMATCHED case and must not consume. */
    private def narrowWalk(target: Type, thisSym: Symbol, escaped: Boolean): WalkResult =
      if (isMoreNarrow(target, thisSym)) Matched(target, consumes = !escaped)
      else containingClassType(target) match {
        case Some(ctx) => narrowWalk(ctx, thisSym, escaped || target.isInstanceOf[ThisType])
        case None      => Unmatched
      }

    /** isMoreNarrow core: the target's class is the same as or an inheritor of the
     *  this-leaf's class — via bases OR self-type (IntelliJ's ScTypeDefinition branch
     *  consults selfType), with abstract types widened to their upper bound. */
    private def isMoreNarrow(target: Type, thisSym: Symbol): Boolean = {
      val cls = classSymOf(target)
      cls == thisSym || cls.isSubClass(thisSym) ||
        (cls.isClass && (cls.typeOfThis.typeSymbol ne cls) && cls.typeOfThis.baseClasses.contains(thisSym))
    }

    /** containingClassType: projection prefixes stay on the spine; a this-type climbs
     *  OUT to the enclosing class's this (the escape hop). */
    private def containingClassType(tp: Type): Option[Type] = tp match {
      case ThisType(sym) =>
        val encl = sym.owner
        if (encl.isClass && !encl.isPackageClass) Some(ThisType(encl)) else None
      case SingleType(pre, _) if pre ne NoPrefix => Some(pre)
      case TypeRef(pre, _, _) if (pre ne NoPrefix) && (pre ne NoType) => Some(pre)
      case _ => None
    }

    // ── guards ───────────────────────────────────────────────────────────────
    /** hasRecursiveThisType0: exact containment OR isSameOrInheritor(rewritten, contained). */
    private def prodGuardBlocks(target: Type, leafSym: Symbol): Boolean =
      target.exists {
        case ThisType(s) => s == leafSym || leafSym.isSubClass(s)
        case _           => false
      }

    /** Progress postcondition, with the leaf-exemption. */
    private def progressBlocks(res: Type, leafSym: Symbol): Boolean = res match {
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
  val EmptySubst = new IjSubst(Vector.empty)

  /** Widen to a class: paths to their underlying, abstract types / type params to
   *  their upper bound (IntelliJ extractClass / TypeParameterType handling). */
  private def classSymOf(t: Type): Symbol = {
    val w = t.widen.dealias
    val s = w.typeSymbol
    if (s.isClass) s
    else if (w.bounds.hi ne w) classSymOf(w.bounds.hi)
    else s
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  TypeDefinitionMembers / MixinNodes analog
  // ═══════════════════════════════════════════════════════════════════════

  case class Sig(member: Symbol, subst: IjSubst)

  /** One inheritance HOP: parent type `pt` as written in `c`'s extends clause.
   *  ParamUpd instantiates the parent's type params with the written args;
   *  ThisUpd re-anchors the parent's this onto the subclass's this. */
  private def hopSubst(c: Symbol, pt: Type): IjSubst = {
    val psym = pt.typeSymbol
    val upds = Vector.newBuilder[Upd]
    if (psym.typeParams.nonEmpty)
      upds += ParamUpd(psym.typeParams.zip(pt.typeArgs).toMap)
    upds += ThisUpd(ThisType(c), Some(psym))
    new IjSubst(upds.result())
  }

  /** Signatures: own decls (terms AND classes — type members ride the same nodes)
   *  with the empty substitutor; inherited decls with the declaring-side substitutor
   *  FOLLOWED BY each hop up the chain (so a hop's replacement is processed by the
   *  remainder — outer instantiations after inner). */
  private def signatures(c: Symbol): List[Sig] = {
    val own = c.info.decls.toList.filter(m => m.isTerm || m.isClass).map(Sig(_, EmptySubst))
    val inherited = c.info.parents.flatMap { pt =>
      val hop = hopSubst(c, pt)
      signatures(pt.typeSymbol).map(s => Sig(s.member, s.subst.followed(hop)))
    }
    val seen = collection.mutable.Set.empty[Name]
    (own ++ inherited).filter(s => seen.add(s.member.name))
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  BaseProcessor analog
  // ═══════════════════════════════════════════════════════════════════════

  /** ScalaResolveState: substitutor + the compoundOrThisType "ugly workaround" flag. */
  case class IjState(substitutor: IjSubst = EmptySubst, compoundOrThis: Boolean = false) {
    def withSubstitutor(s: IjSubst): IjState = copy(substitutor = s)
    def withCompoundOrSelfType: IjState = copy(compoundOrThis = true)
  }

  /** BaseProcessor.RecursionState: the "ugly recursion breakers". */
  case class RecState(visitedProjections: Set[Symbol], visitedTypeParams: Set[Symbol]) {
    def add(sym: Symbol): RecState = copy(visitedProjections = visitedProjections + sym)
    def addTp(sym: Symbol): RecState = copy(visitedTypeParams = visitedTypeParams + sym)
  }
  object RecState { val empty: RecState = RecState(Set.empty, Set.empty) }

  /** The processor: collects (member, substitutor) candidates by name. */
  class NameProcessor(name: Name)(implicit mode: EngineMode) {
    var candidates: List[(Symbol, IjSubst)] = Nil

    def execute(member: Symbol, state: IjState): Boolean = {
      if (member.name == name && !member.isConstructor)
        candidates ::= (member, state.substitutor)
      true
    }

    def processType(t: Type): Unit = processTypeImpl(t, IjState())(RecState.empty)

    // ── BaseProcessor.processTypeImpl: the type dispatch ────────────────────
    def processTypeImpl(t: Type, state: IjState, updateWithProjectionSubst: Boolean = true)
                       (implicit recState: RecState): Boolean = t match {

      // ScThisType(clazz): the self-type branches
      case ThisType(clazz) =>
        val selfTp = clazz.typeOfThis
        val hasSelf = selfTp.typeSymbol ne clazz
        if (!hasSelf)
          processElement(clazz, EmptySubst, state)
        // <:< below stands in for IJ's own conforms() (a separate subsystem)
        else if (selfTp <:< clazz.tpe_*) {
          // self conforms: recurse into the SELF TYPE, substitutor REPLACED (not
          // followed) by ScSubstitutor(ScThisType(clazz), clazz), compound flag set
          val newState = state.withCompoundOrSelfType
            .withSubstitutor(new IjSubst(Vector(ThisUpd(ThisType(clazz), Some(clazz)))))
          processTypeImpl(selfTp, newState)
        }
        else if (clazz.tpe_* <:< selfTp)
          processElement(clazz, EmptySubst, state)
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
          val actualSubst = prefixSignatureSubst(pre, elem)
          val s =
            if (updateWithProjectionSubst)
              actualSubst.followUpdateThisType(st, declarationAnchor(elem))
            else actualSubst
          processElement(elem, s, state)(recState.add(elem))
        }

      // TypeParameterType: recurse the upper bound, projection substs off
      case tr @ TypeRef(_, sym, _) if !sym.isClass && (tr.bounds.hi ne tr) =>
        if (recState.visitedTypeParams.contains(sym)) true
        else processTypeImpl(tr.bounds.hi, state, updateWithProjectionSubst = false)(recState.addTp(sym))

      // ScProjectionType / ParameterizedType over a class designator
      case tr @ TypeRef(pre, cls, args) if cls.isClass =>
        if (recState.visitedProjections.contains(cls)) true
        else {
          val designatorArgs =
            if (args.nonEmpty) new IjSubst(Vector(ParamUpd(cls.typeParams.zip(args).toMap)))
            else EmptySubst
          val actualSubst = prefixSignatureSubst(pre, cls).followed(designatorArgs)
          val s =
            if (updateWithProjectionSubst && (pre ne NoPrefix) && (pre ne NoType))
              actualSubst.followUpdateThisType(tr, declarationAnchor(cls))
            else actualSubst
          processElement(cls, s, state)(recState.add(cls))
        }

      // ScCompoundType: refinement class carries decls + parents via signatures
      case rt: RefinedType =>
        processElement(rt.typeSymbol, EmptySubst, state)

      case NullaryMethodType(res) => processTypeImpl(res, state, updateWithProjectionSubst)

      case _ => true
    }

    // ── BaseProcessor.processElement ────────────────────────────────────────
    private def processElement(e: Symbol, s: IjSubst, state: IjState)
                              (implicit recState: RecState): Boolean = {
      // "val newSubst = if (compoundOrThis.nonEmpty) subst else subst.followed(s)"
      val newSubst = if (state.compoundOrThis) state.substitutor else state.substitutor.followed(s)
      val stateWithSubst = state.withSubstitutor(newSubst).copy(compoundOrThis = false)

      if (e.isClass) {
        // processClassDeclarations: execute every MixinNodes signature with
        // sig.substitutor composed under the accumulated state substitutor
        signatures(e).forall { sig =>
          execute(sig.member, stateWithSubst.withSubstitutor(sig.subst.followed(newSubst)))
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

    /** ScProjectionType.actualSubst analog: the projected element's signature
     *  substitutor from the PREFIX's class (MixinNodes hops), plus the prefix
     *  designator's own type-argument instantiation. */
    private def prefixSignatureSubst(pre: Type, elem: Symbol): IjSubst = {
      if ((pre eq NoPrefix) || (pre eq NoType)) return EmptySubst
      val preCls = classSymOf(pre)
      if (!preCls.isClass || preCls.isPackageClass) return EmptySubst
      val sigSubst = signatures(preCls).find(_.member == elem).map(_.subst).getOrElse(EmptySubst)
      val preArgs = pre.widen.dealias match {
        case TypeRef(_, pc, as) if as.nonEmpty => new IjSubst(Vector(ParamUpd(pc.typeParams.zip(as).toMap)))
        case _ => EmptySubst
      }
      sigSubst.followed(preArgs)
    }

    /** ScSubstitutor.declarationAnchor: the member's containing class. */
    private def declarationAnchor(member: Symbol): Symbol =
      if (member.owner.isClass) member.owner else NoSymbol
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  Reference-level resolution (ReferenceExpressionResolver analog)
  // ═══════════════════════════════════════════════════════════════════════

  /** Resolve `name` against `pre`, then apply the candidate's substitutor with the
   *  reference-level substitutorWithThisType prepend (fromType = pre, seenFromClass
   *  = the member's declaring class).  Returns the resolved SYMBOL too, so callers
   *  never need scalac's own `Type#member` (out of bounds) to continue a path. */
  def ijResolve(pre: Type, name: String)(implicit mode: EngineMode): (Symbol, Type, IjSubst) = {
    val p = new NameProcessor(TermName(name))
    p.processType(pre)
    val (m, subst) = p.candidates.headOption
      .getOrElse(sys.error(s"member $name not found on $pre"))
    val full = subst.followUpdateThisType(pre, m.owner)
    (m, full(m.info).resultType, full)
  }

  def ijMemberType(pre: Type, name: String)(implicit mode: EngineMode): (Type, IjSubst) = {
    val (_, tp, chain) = ijResolve(pre, name)
    (tp, chain)
  }

  /** Qualified-reference chain: each step is a FRESH processor run (as each
   *  reference resolution is in production); recirculation happens through the
   *  computed TYPES (paths for stable members, computed spellings otherwise). */
  def lookupPath(root: Type, names: String*)(implicit mode: EngineMode): (Type, IjSubst) = {
    var pre: Type = root
    var lastType: Type = root
    var lastChain: IjSubst = EmptySubst
    for (name <- names) {
      val (sym, computed, chain) = ijResolve(pre, name)
      lastType = computed
      lastChain = chain
      pre = if (sym.isStable) singleType(pre, sym) else computed
    }
    (lastType, lastChain)
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  ORACLE — the ONLY place out-of-bounds scalac APIs (member/memberType,
  //  i.e. asSeenFrom) may be called.  Fixture/golden path construction in the
  //  tests below may also use them, never the model.
  // ═══════════════════════════════════════════════════════════════════════
  private def scalacMemberType(pre: Type, name: String): Type =
    pre.memberType(pre.member(TermName(name))).resultType

  private def assertParity(pre: Type, name: String)(implicit mode: EngineMode): Unit = {
    val (ij, chain) = ijMemberType(pre, name)
    val oracle = scalacMemberType(pre, name)
    println(s"  [$mode] $pre . $name")
    println(s"     chain : ${chain.render}")
    println(s"     ij    : $ij")
    println(s"     scalac: $oracle")
    assertEquals(s"[$mode] $pre.$name", oracle.toString, ij.toString)
    assertTrue(s"[$mode] $pre.$name =:= (symbols, not just spelling)", ij =:= oracle)
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  Fixtures
  // ═══════════════════════════════════════════════════════════════════════

  // 1. Baseline generics: param subst through one inheritance hop.
  @Test def plainGenerics(): Unit = {
    import ijFixtures._
    println(s"\n=== plainGenerics ===")
    val pre = ThisType(symbolOf[BG])
    for (mode <- allModes) { implicit val m = mode
      assertParity(pre, "foo")
      assertParity(pre, "bar")
    }
  }

  // 2. Two hops: A2[S] extends AG[List[S]]; BG2 extends A2[Int] — foo: List[Int]
  //    requires TWO ParamUpds composing THROUGH the fused chain (T -> List[S],
  //    remainder S -> Int).
  @Test def twoHopGenerics(): Unit = {
    import ijFixtures._
    println(s"\n=== twoHopGenerics ===")
    val pre = ThisType(symbolOf[BG2])
    for (mode <- allModes) { implicit val m = mode
      assertParity(pre, "foo")
    }
  }

  // 3. SCL-7043 shape: this-substs and param-substs interleaved through a
  //    path-dependent lookup (en.values : CEg.this.en.ValueSet).
  @Test def scl7043Shape(): Unit = {
    import ijFixtures._
    println(s"\n=== scl7043Shape ===")
    val ceThis = ThisType(symbolOf[CEg[_]])
    val enPath = singleType(ceThis, ceThis.member(TermName("en")))
    val oracle = scalacMemberType(enPath, "values")
    for (mode <- allModes) { implicit val m = mode
      val (viaPath, chain) = lookupPath(ceThis, "en", "values")
      println(s"  [$mode] chain : ${chain.render}")
      println(s"  [$mode] ij    : $viaPath   scalac: $oracle")
      assertEquals(s"[$mode]", oracle.toString, viaPath.toString)
      assertTrue(s"[$mode] =:=", viaPath =:= oracle)
    }
  }

  // 4. Generic cake: type params AND self-type this-instances in one lookup.
  //    typerG: analyzerG.TyperG where TyperG is an inner class of TypersG[T]
  //    (self: AnalyzerG[T]) — applyG's T must arrive through the type-member
  //    signature hops + the prefix designator's instantiation.
  @Test def genericCake(): Unit = {
    import ijFixtures._
    println(s"\n=== genericCake ===")
    val gThis = ThisType(symbolOf[GlobalG[_]])
    val tPath = singleType(gThis, gThis.member(TermName("typerG")))
    val oracle = {
      val m = tPath.member(TermName("applyG"))
      tPath.memberType(m)
    }
    println(s"  scalac: $oracle")
    for (mode <- allModes) { implicit val m = mode
      val (t, chain) = lookupPath(gThis, "typerG", "applyG")
      println(s"  [$mode] chain[${chain.thisSubstCount} this-substs, dup=${chain.duplicateThisTargets}]: ${chain.render}")
      println(s"  [$mode] ij    : $t")
      assertEquals(s"[$mode]", oracle.resultType.toString, t.toString)
      assertTrue(s"[$mode] =:= (symbols, not just spelling)", t =:= oracle.resultType)
    }
  }

  // 5. THE PUMP through the dispatch: repeated .analyzer/.global reference
  //    resolutions, each a FRESH processor run, recirculating the previous
  //    round's computed spelling as the next round's prefix — production's
  //    actual growth channel.
  @Test def pipelineRounds(): Unit = {
    import inferencerTypes._
    println(s"\n=== pipelineRounds ===")
    val root = ThisType(symbolOf[Analyzer])

    def rounds(n: Int)(implicit mode: EngineMode): Seq[Int] = {
      val (gSym, _, _) = ijResolve(root, "global")
      var pre: Type = singleType(root, gSym)
      (1 to n).map { _ =>
        val (aSym, _, _) = ijResolve(pre, "analyzer")
        val aPath = singleType(pre, aSym)
        val (computed, _) = ijMemberType(aPath, "global")
        pre = computed // recirculate the computed spelling
        spineDepth(computed)
      }
    }

    { implicit val m = Unguarded
      // With path steps resolved through the model's own dispatch (no scalac
      // Type#member shortcut), the unguarded pump compounds INSIDE resolution and
      // ends in StackOverflowError — production's exact fate (60+ segments, SOE
      // from mere descent).  Either observable growth or SOE confirms the pump.
      try {
        val depths = rounds(3)
        println(s"  [$m] depths=${depths.mkString(",")}")
        assertTrue(s"unguarded should pump: $depths",
          depths.zip(depths.tail).forall { case (a, b) => a < b })
      } catch {
        case _: StackOverflowError =>
          println(s"  [$m] StackOverflowError — the pump, terminally (as in production)")
      }
    }
    for (mode <- List(Production, ProgressConsumed)) { implicit val m = mode
      val depths = rounds(4)
      println(s"  [$mode] depths=${depths.mkString(",")}")
      assertEquals(s"[$mode] should fixpoint: $depths", 1, depths.distinct.size)
    }
  }

  private def spineDepth(tp: Type): Int = {
    def go(t: Type, n: Int): Int = t match {
      case SingleType(pre, _) => go(pre, n + 1)
      case TypeRef(pre, _, _) if (pre ne NoPrefix) && (pre ne NoType) => go(pre, n + 1)
      case _ => n
    }
    go(tp, 0)
  }

  // 6. Emergent duplication audit: the layered construction reproduces
  //    duplicate/overlapping chain elements without any hand-crafting.
  @Test def duplicationEmerges(): Unit = {
    import ijFixtures._
    println(s"\n=== duplicationEmerges ===")
    implicit val m: EngineMode = ProgressConsumed
    val ceThis = ThisType(symbolOf[CEg[_]])
    val (t, chain) = lookupPath(ceThis, "en", "values", "toL")
    println(s"  result: $t")
    println(s"  chain[${chain.updates.length} updates, ${chain.thisSubstCount} this-substs, dup=${chain.duplicateThisTargets}]:")
    chain.updates.foreach(u => println(s"    $u"))
    assertTrue("multi-step path lookup should accumulate >= 2 this-substs", chain.thisSubstCount >= 2)

    // and the result must still be right
    val enPath = singleType(ceThis, ceThis.member(TermName("en")))
    val vsType = scalacMemberType(enPath, "values")
    val oracle = scalacMemberType(vsType, "toL")
    println(s"  scalac: $oracle")
    assertEquals(oracle.toString, t.toString)
    assertTrue("=:=", t =:= oracle)
  }

  // 7. The self-type branch of processTypeImpl: looking a member up on
  //    ThisType(trait-with-self-type) recurses into the SELF TYPE with the
  //    substitutor REPLACED by ScSubstitutor(ScThisType(clazz), clazz).
  @Test def selfTypeDispatch(): Unit = {
    import inferencerTypes._
    println(s"\n=== selfTypeDispatch ===")
    val inferThis = ThisType(symbolOf[Infer])
    for (mode <- allModes) { implicit val m = mode
      assertParity(inferThis, "global")
    }
  }

  // 7b. The DEDUP THEOREM: under Progress+Consumed (fall-through + remainder-only
  //     + first-spine-match-per-class), removing equality-duplicate updates from a
  //     chain cannot change any lookup's result — duplicates are provably inert.
  //     This is deliverable (c) closed as a theorem instead of a suite gamble.
  @Test def dedupTheorem(): Unit = {
    import ijFixtures._
    import inferencerTypes._
    println(s"\n=== dedupTheorem ===")
    implicit val m: EngineMode = ProgressConsumed
    val lookups: List[(Type, String)] = List(
      (ThisType(symbolOf[BG]): Type)     -> "foo",
      (ThisType(symbolOf[BG2]): Type)    -> "foo",
      (ThisType(symbolOf[CEg[_]]): Type) -> "en",
      (ThisType(symbolOf[GlobalG[_]]): Type) -> "typerG",
      (ThisType(symbolOf[Infer]): Type)  -> "global")
    for ((pre, name) <- lookups) {
      val (sym, _, chain) = ijResolve(pre, name)
      val full   = chain(sym.info).resultType
      val dedup  = chain.dedupped(sym.info).resultType
      val removed = chain.updates.length - chain.dedupped.updates.length
      println(s"  $pre.$name: ${chain.updates.length} updates, $removed removed by dedup")
      assertEquals(s"$pre.$name dedup must be inert", full.toString, dedup.toString)
      assertTrue(s"$pre.$name dedup =:=", full =:= dedup)
    }
    // NOT removed (correctly): same-target chains under DIFFERENT anchors —
    // the seenFromClass changes the walk, so those are distinct functions
    // (genericCake's sfc=TyperG vs sfc=GlobalG pair stays).
  }
}

object ijFixtures {
  // 1 & 2: generics
  trait AG[T] { def foo: T; def bar(t: T): T }
  abstract class BG extends AG[Int]
  trait A2[S] extends AG[List[S]]
  abstract class BG2 extends A2[Int]

  // 3: SCL-7043 shape (aggregation of an Enumeration-like with inner path-dependent types)
  class MyEnum {
    class Value
    class ValueSet { def toL: List[MyEnum.this.Value] = Nil }
    def values: ValueSet = new ValueSet
  }
  class CEg[T <: MyEnum](val en: T)

  // 4: generic cake — type params and self-type this-instances in one lookup
  trait TypersG[T] { self: AnalyzerG[T] =>
    class TyperG { def applyG(t: T): T = t }
  }
  trait InferG[T] { self: AnalyzerG[T] =>
    class InferencerG { def infer(t: T): T = t }
  }
  trait AnalyzerG[T] extends TypersG[T] with InferG[T] { val globalG: GlobalG[T] }
  class GlobalG[T] {
    lazy val analyzerG: AnalyzerG[T] = ???
    lazy val typerG: analyzerG.TyperG = ???
  }
}
