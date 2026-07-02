package scala.reflect.internal

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

/**
 * A replica of IntelliJ-Scala's member lookup WITH substitution, built over scalac
 * Types/Symbols, so its limitations and scalac-parity can be reasoned about in one
 * place with scalac's own `memberType` as the oracle.
 *
 * The construction layers are modeled structurally (not as hand-written chains), so
 * the duplicate-substitutor chains observed in the real traces EMERGE here:
 *
 *   1. MixinNodes.SuperTypesData analog (`signatures`): a template's inherited
 *      signatures carry a substitutor composed RECURSIVELY along the parent chain —
 *      one (ParamUpd?, ThisUpd) pair per inheritance hop.  Composition order puts
 *      the DECLARING side first and each hop AFTER it, so a hop's replacement is
 *      processed by the remainder (outer instantiations see inner output).
 *   2. Projection/parameterized substitutor analog (`contextParamSubst`): type
 *      params of the member's declaring & enclosing classes instantiated from the
 *      lookup prefix via `baseType` — IntelliJ's ScProjectionType/ScParameterizedType
 *      contribution.
 *   3. Resolve-state prepending analog (`processType`): each lookup PREPENDS
 *      ThisUpd(fromType, seenFromClass = declaring class) — `substitutorWithThisType`.
 *   4. `followed` is blind concatenation, and path lookups (`lookupPath`) carry the
 *      previous step's whole chain forward as state — the recirculation channel that
 *      makes chains accumulate and duplicate.
 *
 * The engine (`IjSubst.apply`) mirrors ScSubstitutor.recursiveUpdateImpl: the first
 * matching update REPLACES a leaf and the REMAINDER of the chain processes the
 * replacement; non-matching nodes descend with the full remaining chain.  The
 * this-walk mirrors ThisTypeSubstitution: anchored lockstep (target baseType clazz)
 * .prefix climb, narrowing (isMoreNarrow) fallback, and the escape-climb through
 * enclosing this-types.
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
  //  Engine
  // ═══════════════════════════════════════════════════════════════════════

  case class EngineMode(prodGuard: Boolean, progress: Boolean, consumed: Boolean) {
    override def toString =
      if (prodGuard) "Production"
      else if (progress && consumed) "Progress+Consumed"
      else if (progress) "Progress"
      else if (consumed) "Consumed"
      else "Unguarded"
  }
  val Production       = EngineMode(prodGuard = true,  progress = false, consumed = false)
  val Unguarded        = EngineMode(prodGuard = false, progress = false, consumed = false)
  val ProgressOnly     = EngineMode(prodGuard = false, progress = true,  consumed = false)
  val ProgressConsumed = EngineMode(prodGuard = false, progress = true,  consumed = true)
  val allModes = List(Production, Unguarded, ProgressOnly, ProgressConsumed)

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
   *  their upper bound (IntelliJ extractClass / isMoreNarrow alias handling). */
  private def classSymOf(t: Type): Symbol = {
    val w = t.widen.dealias
    val s = w.typeSymbol
    if (s.isClass) s
    else if (w.bounds.hi ne w) classSymOf(w.bounds.hi)
    else s
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  Layer 1: MixinNodes.SuperTypesData analog
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

  /** Signatures: own decls with the empty substitutor; inherited decls with the
   *  declaring-side substitutor FOLLOWED BY each hop up the chain (so a hop's
   *  replacement is processed by the remainder — outer instantiations after inner). */
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
  //  Layer 2: projection/parameterized substitutor analog
  // ═══════════════════════════════════════════════════════════════════════

  /** Instantiate type params of the member's declaring class and its ENCLOSING
   *  classes from the lookup prefix: declaring-class params via
   *  `fromType baseType E`, enclosing-class params via the prefix chain
   *  (IntelliJ's ScProjectionType/ScParameterizedType substitutor contribution;
   *  scalac's classParameterAsSeen does the same climb inside the map). */
  private def contextParamSubst(fromType: Type, member: Symbol): Vector[Upd] = {
    val upds = Vector.newBuilder[Upd]
    var e = member.owner
    var seat: Type = fromType // where E's params are instantiated from
    while (e != NoSymbol && e.isClass && !e.isPackageClass) {
      if (e.typeParams.nonEmpty) {
        val bt = seat.baseType(e) match {
          case NoType =>
            // enclosing rather than inherited: instantiate from the prefix
            val w = seat.widen
            if ((w.prefix ne NoType) && (w.prefix ne NoPrefix)) w.prefix.baseType(e) else NoType
          case t => t
        }
        if (bt != NoType && bt.typeArgs.nonEmpty)
          upds += ParamUpd(e.typeParams.zip(bt.typeArgs).toMap)
      }
      e = e.owner
    }
    upds.result()
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  Layer 3 + 4: resolve-state prepending and path lookup (recirculation)
  // ═══════════════════════════════════════════════════════════════════════

  /** BaseProcessor.processTypeImpl + substitutorWithThisType:
   *  chain = ThisUpd(fromType, sfc=declaring) +: contextParams ++: sigSubst ++: state. */
  private def processType(fromType: Type, name: Name, state: IjSubst): (Symbol, IjSubst) = {
    val cls = classSymOf(fromType)
    val sig = signatures(cls).find(_.member.name == name)
      .getOrElse(sys.error(s"member $name not found in $cls"))
    val declaring = sig.member.owner
    val chain = new IjSubst(contextParamSubst(fromType, sig.member))
      .followed(sig.subst)
      .followed(state)
      .followUpdateThisType(fromType, declaring)
    (sig.member, chain)
  }

  /** ijMemberType: the model's `pre.memberType(name).resultType`. */
  def ijMemberType(pre: Type, name: String, state: IjSubst = EmptySubst)(implicit mode: EngineMode): (Type, IjSubst) = {
    val (m, chain) = processType(pre, TermName(name), state)
    (chain(m.info).resultType, chain)
  }

  /** Path lookup with state carry-forward: the previous step's WHOLE chain is the
   *  next step's state (the recirculation/duplication channel).  Stable members
   *  extend the path prefix; others contribute their computed type. */
  def lookupPath(root: Type, names: String*)(implicit mode: EngineMode): (Type, IjSubst) = {
    var pre: Type = root
    var state: IjSubst = EmptySubst
    var lastType: Type = root
    for (name <- names) {
      val (m, chain) = processType(pre, TermName(name), state)
      val computed = chain(m.info).resultType
      lastType = computed
      pre = if (m.isStable) singleType(pre, pre.member(TermName(name))) else computed
      state = chain
    }
    (lastType, state)
  }

  /** Oracle. */
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
    }
  }

  // 4. Generic cake: type params AND self-type this-instances in one lookup.
  //    typerG: analyzerG.TyperG where TyperG is an inner class of TypersG[T]
  //    (self: AnalyzerG[T]) — applyG's T must come from the ENCLOSING class's
  //    instantiation through the prefix, its this from the cake.
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

  // 5. THE PUMP EMERGES from the layered pipeline: repeated .analyzer/.global
  //    rounds with the accumulated state carried forward reproduce the growth
  //    channel structurally — Unguarded grows SUPER-linearly (3, 9, 25, 67: the
  //    carried chain re-embeds the recirculated prefix at every round), while the
  //    guarded modes hold a fixpoint.  Production's fixpoint chain carries
  //    duplicate targets (dup=true) — the accidental truncation the real traces
  //    showed; the probes fixpoint by blocking/consuming instead.
  @Test def pipelineRounds(): Unit = {
    import inferencerTypes._
    println(s"\n=== pipelineRounds ===")
    val root = ThisType(symbolOf[Analyzer])

    def rounds(n: Int)(implicit mode: EngineMode): (Seq[Int], IjSubst) = {
      var pre: Type = singleType(root, root.member(TermName("global")))
      var state: IjSubst = EmptySubst
      val depths = (1 to n).map { _ =>
        val aPath = singleType(pre, pre.member(TermName("analyzer")))
        val (m1, c1) = processType(pre, TermName("analyzer"), state)
        val (m2, c2) = processType(aPath, TermName("global"), c1)
        val computed = c2(m2.info).resultType
        pre = computed // recirculate the computed spelling as the next round's prefix
        state = c2
        spineDepth(computed)
      }
      (depths, state)
    }

    { implicit val m = Unguarded
      val (depths, st) = rounds(4)
      println(s"  [$m] depths=${depths.mkString(",")}  final chain: ${st.updates.length} updates, ${st.thisSubstCount} this-substs, dup=${st.duplicateThisTargets}")
      assertTrue(s"unguarded pipeline should pump: $depths",
        depths.zip(depths.tail).forall { case (a, b) => a < b })
    }
    for (mode <- List(Production, ProgressOnly, ProgressConsumed)) { implicit val m = mode
      val (depths, st) = rounds(4)
      println(s"  [$mode] depths=${depths.mkString(",")}  final chain: ${st.updates.length} updates, ${st.thisSubstCount} this-substs, dup=${st.duplicateThisTargets}")
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
  //    duplicate-target chains without any hand-crafting.
  @Test def duplicationEmerges(): Unit = {
    import ijFixtures._
    println(s"\n=== duplicationEmerges ===")
    implicit val m: EngineMode = ProgressConsumed
    val ceThis = ThisType(symbolOf[CEg[_]])
    val (t, chain) = lookupPath(ceThis, "en", "values", "toL")
    println(s"  result: $t")
    println(s"  chain[${chain.updates.length} updates, ${chain.thisSubstCount} this-substs, dup=${chain.duplicateThisTargets}]:")
    chain.updates.foreach(u => println(s"    $u"))
    assertTrue("multi-step path lookup should accumulate >= 3 this-substs", chain.thisSubstCount >= 3)

    // and the result must still be right
    val enPath = singleType(ceThis, ceThis.member(TermName("en")))
    val vsType = scalacMemberType(enPath, "values")
    val oracle = scalacMemberType(vsType, "toL")
    println(s"  scalac: $oracle")
    assertEquals(oracle.toString, t.toString)
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
