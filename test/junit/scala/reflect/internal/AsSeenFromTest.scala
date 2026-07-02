package scala.reflect.internal

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{After, Assert, Before, Test}

import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.language.existentials
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class AsSeenFromTest {

  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._
  import definitions._

  type EmptyList[A] = Nil.type

  class ann[A] extends StaticAnnotation

  trait A[B] {
    def foo: B
    trait A_I {
      def foo: B
    }
  }

  trait B extends A[Int] {
    trait B_I extends A_I
  }

  trait C extends A[Int] {
    trait B_I extends A[String]
  }
  @Test
  def asSeenFrom(): Unit = {
    assertEquals(typeOf[Int], fooResult(typeOf[B]))
    assertEquals(typeOf[Int], fooResult(typeOf[B#B_I]))
    assertEquals(typeOf[String], fooResult(typeOf[C#B_I]))
  }

  object asSeenFrom2Types {

    trait O1[O1_A] {
      type O1_A_Alias = O1_A
      trait I1[I1_A] {
        type I1_A_Alias = I1_A
        def foo: O1_A_Alias
        def bar(x: O1_A_Alias): Unit
      }
    }

    trait O2 extends O1[Int] {
      trait I2 extends I1[String] with O1[Nothing] {
        self: X =>
        bar(self.foo)
      }
    }
    trait X
    val o2: O2 = ???
    val pre: o2.I2 with X = ???
  }

  @Test def asSeenFrom2(): Unit = {
    import asSeenFrom2Types._
    val seenFronTyoe = fooResult(ThisType(symbolOf[O2#I2]))
    assertEquals(TypeRef(ThisType(symbolOf[O2]), typeOf[O1[_]].member(TypeName("O1_A_Alias")), Nil), seenFronTyoe)
    assertTrue(typeOf[Int] =:= seenFronTyoe)
  }

  @Test def t21585(): Unit = {
    import t21585Types._

    // apply(O1.this.O1_A_Alias : NullaryMethodType)
    //  apply(O1.this.O1_A_Alias : AliasNoArgsTypeRef)
    //    apply(O1.this.type : UniqueThisType)
    //      thisTypeAsSeen(O1.this.type)
    //        matchesPrefixAndClass(pre=I2.this.type, class=trait I1)(candidate=trait O1)
    //        = false
    //        matchesPrefixAndClass(pre=O2.this.type, class=trait O1)(candidate=trait O1)
    //        = true
    //      = O2.this.type
    //    = O2.this.type
    //  = O2.this.O1_A_Alias
    //= O2.this.O1_A_Alias
    val seenFromType = fooResult(ThisType(typeOf[O2].member(TypeName("I2"))))
    assertEquals(TypeRef(ThisType(symbolOf[O2]), typeOf[O1[_]].member(TypeName("O1_A_Alias")), Nil), seenFromType)
    assertTrue(typeOf[Int] =:= seenFromType)
  }

  private def fooResult(pre: Type) = {
    val member = pre.member(TermName("foo"))
    tracedAsSeenFrom(pre, member).resultType
  }

  /** Trace `member.info` asSeenFrom `pre` (clazz = member.owner), printing every
   *  apply / thisTypeAsSeen / matchesPrefixAndClass / classParameterAsSeen step. */
  private def tracedAsSeenFrom(pre: Type, member: Symbol): Type = {
    val tpe = member.info
    println(s"== asSeenFrom: member=$member owner=${member.owner} info=$tpe ==")
    println(s"   pre=$pre (${pre.getClass.getSimpleName})")
    class LoggingAsSeenFromMap(seenFromPrefix: Type, seenFromClass: Symbol, debug: Boolean = false, var indentLevel: Int = 0)
      extends AsSeenFromMap(seenFromPrefix, seenFromClass) {

      def logged[T](message: String, op: => T): T = if (debug) {
        println(s"${"  " * indentLevel}${message.replace('\n', ' ')}")
        indentLevel += 1
        val result = op
        indentLevel -= 1
        println(s"${"  " * indentLevel}= $result")
        result
      } else op

      override def apply(tp: Type): Type = {
        logged(s"apply($tp : ${tp.getClass.getSimpleName})", super.apply(tp))
      }

      override protected def correspondingTypeArgument(lhs: Type, rhs: Type): Type =
        logged(s"correspondingTypeArgument($lhs, $rhs)", super.correspondingTypeArgument(lhs, rhs))
      override protected def matchesPrefixAndClass(pre: Type, clazz: Symbol)(candidate: Symbol): Boolean =
        logged(s"matchesPrefixAndClass(pre=$pre, class=$clazz)(candidate=$candidate)", super.matchesPrefixAndClass(pre, clazz)(candidate))
      override protected def classParameterAsSeen(classParam: TypeRef): Type =
        logged(s"classParameterAsSeen($classParam)", super.classParameterAsSeen(classParam))
      override protected def thisTypeAsSeen(tp: ThisType): Type =
        logged(s"thisTypeAsSeen($tp)", super.thisTypeAsSeen(tp))
    }
    new LoggingAsSeenFromMap(pre, member.owner, debug = true).apply(tpe)
  }

  // SCL-21947 Inferencer shape (scalac-green). Mirrors the IntelliJ fixture:
  //   typer.applyTypeToWildcards(pattp), where `typer` is `object typer extends
  //   analyzer.Typer`, reached through the abstract `val global: Global`.
  @Test def inferencer(): Unit = {
    import inferencerTypes._
    val analyzerThis = ThisType(symbolOf[Analyzer])
    val globalSym    = analyzerThis.member(TermName("global"))
    val globalPre    = singleType(analyzerThis, globalSym)
    println(s"globalPre=$globalPre underlying=${globalPre.widen}")
    val typerSym     = globalPre.member(TermName("typer"))
    println(s"typerSym=$typerSym : ${typerSym.info}")
    val typerPre     = singleType(globalPre, typerSym)
    println(s"typerPre=$typerPre")
    val member       = typerPre.member(TermName("applyTypeToWildcards"))
    println(s"member=$member : ${member.info}")
    val result       = tracedAsSeenFrom(typerPre, member)
    println(s"RESULT param type = ${result.paramTypes.headOption.getOrElse(result)}")
  }

  // The IntelliJ "remnant growth" example, re-run through scalac. IntelliJ's
  // resolution iterates: select .analyzer, select .global, re-derive members FROM
  // THE PREVIOUS ROUND'S OUTPUT — growing analyzer.global spellings without bound
  // (60+ segments) unless its hasRecursiveThisType guard blocks. scalac's
  // invariant, demonstrated below, is ROUND-TRIP NEUTRALITY:
  //   underlying(pre.analyzer.global) == pre    -- for ANY pre, even a deep one.
  // The refinement member's info is re-anchored ONCE to the selection prefix (the
  // copied-refinement decls minted by the cached widen/asSeenFrom), so underlying
  // strips exactly the layer selection added: net growth per round is ZERO, and
  // iteration is a fixpoint. Note scalac does NOT eagerly canonicalize a deep
  // spelling down to P0 — it merely never grows one.
  @Test def remnantFixpoint(): Unit = {
    import inferencerTypes._
    val analyzerThis = ThisType(symbolOf[Analyzer])
    val P0 = singleType(analyzerThis, analyzerThis.member(TermName("global")))
    println(s"P0 = $P0")

    var pre: Type = P0
    for (round <- 1 to 3) {
      println(s"\n===== ROUND $round: pre = $pre =====")
      val analyzerPath = singleType(pre, pre.member(TermName("analyzer")))
      println(s"select .analyzer -> $analyzerPath")
      val globalSym = analyzerPath.member(TermName("global"))
      println(s"member 'global' info = ${globalSym.info}   // re-anchored ONCE to the selection prefix by the copied-refinement widen; not accumulated across rounds")
      val under = tracedAsSeenFrom(analyzerPath, globalSym).resultType
      println(s"underlying(pre.analyzer.global) = $under")
      pre = under // scalac's next-round input: the canonical underlying
    }

    // The exact IntelliJ poison step: rewrite Infer.this against a path whose
    // class (Analyzer) inherits Infer. scalac performs the SAME defensible
    // rewrite — once — and the result is consumed, never recirculated:
    val analyzerPath = singleType(P0, P0.member(TermName("analyzer")))
    val inferThis    = ThisType(symbolOf[Infer])
    println(s"\n===== POISON STEP: Infer.this asSeenFrom (pre=$analyzerPath, clazz=Infer) =====")
    println(s"= ${inferThis.asSeenFrom(analyzerPath, symbolOf[Infer])}")

    // And a pre-built DEEP spelling (what IntelliJ accumulates): round-trip
    // neutrality holds at any depth — underlying(deep.analyzer.global) == deep,
    // i.e. one more selection round nets ZERO growth (it does not eagerly shrink
    // the deep spelling either; comparison-time underlying-chasing equates it
    // with P0 lazily).
    var deep: Type = P0
    for (_ <- 1 to 3) {
      deep = singleType(deep, deep.member(TermName("analyzer")))
      deep = singleType(deep, deep.member(TermName("global")))
    }
    println(s"\n===== DEEP SPELLING (3x, built by hand): $deep =====")
    val deepAnalyzer = singleType(deep, deep.member(TermName("analyzer")))
    val under = tracedAsSeenFrom(deepAnalyzer, deepAnalyzer.member(TermName("global"))).resultType
    println(s"underlying(deep.analyzer.global) = $under   // == deep: one layer stripped, exactly what selection added")
  }

  // =========================================================
  // FusedSubstMap: scalac model of IntelliJ ScSubstitutor.recursiveUpdateImpl
  // =========================================================
  // IntelliJ holds substitution chains as Array[Update].  On match the REMAINDER
  // of the chain processes the replacement (leaf→path contract violated by design).
  // The anchorless heuristic (doUpdateThisType / isMoreNarrow) returns the WHOLE
  // target when target.baseType(thisSym) != NoType — even if the target IS rooted
  // at that same this-type, causing self-embedding: Analyzer.this → P where P
  // contains Analyzer.this, so the result still contains Analyzer.this and the
  // next round grows P by one more selection.
  //
  // scalac's invariant (remnantFixpoint): no growth because round-trip neutrality —
  //   underlying(pre.analyzer.global) == pre  for any pre.
  // The refinement member's info is anchored ONCE to the selection prefix by the
  // cached widen/asSeenFrom; net growth per round = 0, iteration is a fixpoint.
  // IntelliJ lacks this mechanism and recirculates grown paths as fresh targets.

  sealed abstract class FusedUpd
  case class FThisUpd(target: Type, anchor: Option[Symbol]) extends FusedUpd
  case class FParamUpd(from: List[Symbol], to: List[Type]) extends FusedUpd

  /** Scalac approximation of IntelliJ's anchorless doUpdateThisType:
   *  Climb the target's prefix spine; return the first t s.t.
   *  t.baseType(thisSym) != NoType.  If the top of the spine qualifies,
   *  the WHOLE target is returned — which may still contain thisSym.type. */
  private def anchorlessMatch(target: Type, thisSym: Symbol): Option[Type] = {
    @annotation.tailrec
    def climb(t: Type): Option[Type] = t match {
      case NoType | NoPrefix => None
      case _ if t.baseType(thisSym) != NoType => Some(t)
      case _ => climb(t.prefix)
    }
    climb(target)
  }

  /** IntelliJ's anchored doUpdateThisTypeFromClass, faithfully:
   *  - terminal cases (no anchor / anchor == the leaf's own class / anchor is
   *    top-level) fall into doUpdateThisType — the NARROWING climb, which may
   *    return a stripped prefix of the target, not the whole target;
   *  - otherwise lockstep: (target baseType clazz).prefix, clazz := clazz.owner;
   *  - clazz not a base of target → "narrow against pre" fallthrough (SCL-21947
   *    OuterPathTransformer shape). */
  private def anchoredMatch(target: Type, clazz: Symbol, thisSym: Symbol): Option[Type] =
    if (clazz == NoSymbol || clazz == thisSym || !clazz.owner.isClass)
      anchorlessMatch(target, thisSym)
    else {
      val bt = target.baseType(clazz)
      if (bt == NoType) anchorlessMatch(target, thisSym)
      else anchoredMatch(bt.prefix, clazz.owner, thisSym)
    }

  // ── Guard modes ────────────────────────────────────────────────────────────
  // A guard decides, per this-rewrite firing, whether the rewrite is admitted.
  // Pre-guards inspect the TARGET before matching (IntelliJ's shape); the
  // Progress post-guard inspects the RETURNED type (a progress/non-circularity
  // postcondition, scalac-inspired: thisTypeAsSeen only ever strips prefixes
  // from pre, so its output can never still be rooted in the this it eliminates).
  // Falsified candidates removed after evaluation (see git history):
  //   Arm1Exact (target contains ThisType(leafSym)) and RootOfSpineExact
  //   (leafSym is the target's spine root) — both MISS the cross-symbol pump,
  //   where the rewritten leaf's class differs from the target's root symbol.
  //   Terminal-output, depth caps, and chunked passes fell earlier (see the
  //   intellij-scala scala-typesystem-tck branch ledger).
  sealed trait GuardMode
  case object NoGuard          extends GuardMode
  /** Faithful IntelliJ hasRecursiveThisType0 (the incumbent): block if target
   *  contains a this-type t with leafSym == t.sym OR leafSym.isSubClass(t.sym)
   *  (isSameOrInheritor — note the DIRECTION: rewritten class inherits the
   *  contained this's class, NOT the other way around).  O(type-size). */
  case object ProdGuard        extends GuardMode
  /** POST-guard: block if the RETURNED type's spine root is ThisType(c) with
   *  c.isSubClass(leafSym) — i.e. the rewrite claims to eliminate leafSym.this
   *  but returns a type still rooted in a this-type denoting that same instance
   *  (via inheritance).  No progress => self-embedding => recirculation fuel.
   *  O(depth of output) + one subclass test. */
  case object Progress         extends GuardMode

  /** Innermost this-type on the prefix spine (or the spine top if none). */
  private def spineRoot(t: Type): Type = t match {
    case _: ThisType => t
    case _ => t.prefix match {
      case NoType | NoPrefix => t
      case p                 => spineRoot(p)
    }
  }

  private def containsThisWhere(tp: Type, p: Symbol => Boolean): Boolean =
    tp.exists { case ThisType(s) => p(s); case _ => false }

  private def preBlocked(mode: GuardMode, target: Type, leafSym: Symbol): Boolean = mode match {
    case ProdGuard => containsThisWhere(target, s => s == leafSym || leafSym.isSubClass(s))
    case _         => false
  }

  private def postBlocked(mode: GuardMode, result: Type, leafSym: Symbol): Boolean = mode match {
    case Progress => spineRoot(result) match {
      case ThisType(c) => c.isSubClass(leafSym)
      case _           => false
    }
    case _ => false
  }

  /** Apply fused chain: first matching update REPLACES the leaf, the remainder
   *  of the chain is applied to the replacement.  Non-matching nodes descend
   *  with the FULL chain (mirroring IntelliJ recursiveUpdateImpl).  A blocked
   *  firing behaves like a non-match: the leaf is kept and the REST of the
   *  chain still gets to try it (as in ScSubstitutor: an undefined subst PF
   *  falls through to the next update). */
  private def applyFused(tp: Type, upds: List[FusedUpd], mode: GuardMode = NoGuard): Type = {
    def tryMatch(t: Type, us: List[FusedUpd]): Option[(Type, List[FusedUpd])] = us match {
      case Nil => None
      case u :: rest =>
        val hit: Option[Type] = u match {
          case FThisUpd(target, anchorOpt) => t match {
            case ThisType(sym) if !preBlocked(mode, target, sym) =>
              val r = anchorOpt match {
                case Some(clazz) => anchoredMatch(target, clazz, sym)
                case None        => anchorlessMatch(target, sym)
              }
              r.filterNot(postBlocked(mode, _, sym))
            case _ => None
          }
          case FParamUpd(from, to) => t match {
            case TypeRef(NoPrefix, sym, Nil) if from.contains(sym) =>
              Some(to(from.indexOf(sym)))
            case _ => None
          }
        }
        hit match {
          case Some(r) => Some((r, rest))
          case None    => tryMatch(t, rest)
        }
    }
    tryMatch(tp, upds) match {
      case Some((replacement, rest)) => applyFused(replacement, rest, mode)
      case None =>
        val descend = new TypeMap { def apply(t: Type) = applyFused(t, upds, mode) }
        descend.mapOver(tp)
    }
  }

  /** Count prefix-spine hops from tp upward. */
  private def spineDepth(tp: Type): Int = {
    @annotation.tailrec
    def go(t: Type, n: Int): Int = t.prefix match {
      case NoType | NoPrefix => n
      case p                 => go(p, n + 1)
    }
    go(tp, 0)
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Fixture 1 — the pump, as the FUSED PAIR (the actual IntelliJ chain shape).
  // (A single anchorless update pumps just the same — fusion is not required for
  // self-embedding — the pair is kept because it also exercises the anchored
  // strip + remainder composition.)
  //
  // One round of the observed pump is a fused chain of two this-substitutions
  // sharing the same recirculated target P (chain-audit showed duplicate targets):
  //   [update 1/2]  Global.this  -> anchored narrow against P  => STRIPS to P.prefix
  //                 (benign: the climb removes the trailing .analyzer)
  //   [update 2/2]  the REMAINDER processes update 1's output: its spine root
  //                 Analyzer.this matches the anchorless update => WHOLE P returned
  //                 (self-embedding: P is rooted at Analyzer.this)
  // then resolution projects .analyzer onto the output and re-mints it as the
  // next round's target:  nextP = singleType(result, analyzer).
  //
  // Neutrality, made visible: when the poison firing is blocked, the round is
  // result = P.prefix, nextP = P.prefix.analyzer == P  — BYTE-IDENTICAL fixpoint,
  // exactly scalac's underlying(pre.analyzer.global) == pre invariant emerging
  // from the guarded engine.  Without a guard: +2 spine segments per round.
  // ─────────────────────────────────────────────────────────────────────────
  @Test def fusedPairPump(): Unit = {
    import inferencerTypes._
    val analyzerThis = ThisType(symbolOf[Analyzer])
    val globalPath   = singleType(analyzerThis, analyzerThis.member(TermName("global")))
    val P0           = singleType(globalPath, globalPath.member(TermName("analyzer")))
    val globalLeaf: Type = ThisType(symbolOf[Global])

    def round(P: Type, mode: GuardMode): Type = {
      val chain  = List(FThisUpd(P, Some(symbolOf[Global])), FThisUpd(P, None))
      val result = applyFused(globalLeaf, chain, mode)
      // recirculation: resolution projects .analyzer onto the substituted output
      singleType(result, result.member(TermName("analyzer")))
    }

    println(s"\n=== fusedPairPump ===  P0 = $P0 (depth ${spineDepth(P0)})")

    // No guard: the pair pumps.
    var p: Type = P0
    val depths = (1 to 4).map { r =>
      p = round(p, NoGuard)
      println(s"  NoGuard round $r: depth=${spineDepth(p)}  $p")
      spineDepth(p)
    }
    for (i <- 0 until depths.length - 1)
      assertTrue(s"pair should pump: ${depths.mkString(",")}", depths(i) < depths(i + 1))

    // Every guard mode: byte-identical fixpoint — the neutrality invariant.
    for (mode <- List[GuardMode](ProdGuard, Progress)) {
      var q: Type = P0
      for (r <- 1 to 4) {
        val next = round(q, mode)
        assertEquals(s"$mode round $r: expected byte-identical fixpoint", q.toString, next.toString)
        assertTrue(s"$mode round $r: =:= fixpoint", next =:= q)
        q = next
      }
      println(s"  ✓ $mode: byte-identical fixpoint at $q")
    }
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Fixture 1c — the CROSS-SYMBOL pump.
  //
  // Same mechanism, but the this-leaf being rewritten (Infer.this — fresh each
  // round from member declarations in trait Infer) is NOT the symbol at the
  // target's spine root (Analyzer.this).  The anchorless heuristic still fires
  // (Analyzer inherits Infer => whole target), so the pump runs — and the
  // FAITHFUL PRODUCTION guard misses it: its inheritor arm tests
  // leafSym.isSubClass(containedThis.sym) = Infer <:< Analyzer = FALSE — the
  // INVERTED direction from what the pump needs.  Only Progress blocks:
  // root(returned) = Analyzer.this, Analyzer <:< Infer.
  //
  // In the observed IntelliJ trace the recirculated targets happen to be
  // SPELLED with the same root symbol as the rewritten leaf (Infer.this....),
  // which is why exact arm-1 catches the real pump.  Nothing in the engine
  // guarantees that alignment; this fixture is the misaligned variant.  Whether
  // the shape is reachable in real IntelliJ resolution is an open question to
  // test there — if it is, production has an unguarded pump.
  // ─────────────────────────────────────────────────────────────────────────
  @Test def crossSymbolPump(): Unit = {
    import inferencerTypes._
    val analyzerThis = ThisType(symbolOf[Analyzer])
    val globalPath   = singleType(analyzerThis, analyzerThis.member(TermName("global")))
    val P0           = singleType(globalPath, globalPath.member(TermName("analyzer")))
    val inferLeaf: Type = ThisType(symbolOf[Infer])

    def round(P: Type, mode: GuardMode): Type = {
      val result = applyFused(inferLeaf, List(FThisUpd(P, None)), mode)
      val gPath  = singleType(result, result.member(TermName("global")))
      singleType(gPath, gPath.member(TermName("analyzer")))
    }

    println(s"\n=== crossSymbolPump ===  P0 = $P0")

    def depthsFor(mode: GuardMode): Seq[Int] = {
      var p: Type = P0
      (1 to 4).map { r =>
        p = round(p, mode)
        println(s"  $mode round $r: depth=${spineDepth(p)}  $p")
        spineDepth(p)
      }
    }

    for (mode <- List[GuardMode](NoGuard, ProdGuard)) {
      val ds = depthsFor(mode)
      for (i <- 0 until ds.length - 1)
        assertTrue(s"$mode should MISS the cross-symbol pump (grows): ${ds.mkString(",")}", ds(i) < ds(i + 1))
      println(s"  ✗ $mode missed the pump (as predicted): depths ${ds.mkString(", ")}")
    }

    val dsProgress = depthsFor(Progress)
    assertEquals(s"Progress should fixpoint: ${dsProgress.mkString(",")}", 1, dsProgress.distinct.size)
    println(s"  ✓ Progress blocked the cross-symbol pump: fixpoint at depth ${dsProgress.head}")
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Fixture 2 — the LEGITIMATE sequential re-anchor (SCL-7043's shape).
  //
  // From the captured IntelliJ trace (testScratchSCL7043Trace/Prod): the load-
  // bearing chain is
  //   [update 1/2]  Enumeration.this -> CE.this.enum.type   (sfc=Enumeration)
  //   [update 2/2]  CE.this          -> <outer path>         (sfc=CE)
  // where update 2 MUST process update 1's output to re-anchor the freshly
  // introduced CE.this onto the concrete instance path — the sequential
  // composition a rewriting update's remainder must be allowed to perform
  // (this was terminal-output's 1-in-592 counterexample; see git history).
  //
  // Every guard admits both rewrites here:
  //   update 1 returns CE.this.en.type — root CE.this, and CE does NOT inherit
  //   Enumeration (it AGGREGATES one: val en: Enumeration), so Progress allows;
  //   arm-1/prod allow (no Enumeration.this inside the target); root-of-spine
  //   allows (root CE.this ≠ Enumeration.this).
  //   update 2 returns host.ce.type — no this-type on its spine at all.
  // The discriminating line: inheritance-rooted returns are recirculation fuel;
  // aggregation-rooted returns are honest re-anchors.
  // ─────────────────────────────────────────────────────────────────────────
  @Test def sequentialReanchor(): Unit = {
    import scl7043Types._
    val ceThis   = ThisType(symbolOf[CE])
    val enumPath = singleType(ceThis, ceThis.member(TermName("en")))         // CE.this.en.type
    val hostPre  = typeOf[host.type]
    val cePath   = singleType(hostPre, hostPre.member(TermName("ce")))       // host.ce.type
    val enumLeaf: Type = ThisType(typeOf[Enumeration].typeSymbol)

    println(s"\n=== sequentialReanchor ===")
    println(s"  enumPath=$enumPath  cePath=$cePath")

    val chain = List(
      FThisUpd(enumPath, Some(typeOf[Enumeration].typeSymbol)),
      FThisUpd(cePath, None))

    // Expected: Enumeration.this -> CE.this.en.type -> (remainder) -> host.ce.en.type
    val expected = singleType(cePath, cePath.member(TermName("en")))

    for (mode <- List[GuardMode](NoGuard, ProdGuard, Progress)) {
      val r = applyFused(enumLeaf, chain, mode)
      println(s"  $mode: $r")
      assertEquals(s"$mode must admit the sequential re-anchor", expected.toString, r.toString)
      assertTrue(s"$mode result =:= expected", r =:= expected)
    }
    println(s"  ✓ all guards admit the re-anchor: $expected")
  }
}
object scl7043Types {
  class CE(val en: Enumeration)
  object host { val ce: CE = new CE(null) }
}
object inferencerTypes {
  trait Typers { self: Analyzer =>
    import global._
    abstract class Typer {
      def applyTypeToWildcards(tp: Type): Type = tp
    }
  }
  trait Infer { self: Analyzer =>
    import global._
    class Inferencer {
      def inferTypedPattern(pattp: Type): Type =
        typer.applyTypeToWildcards(pattp)
    }
  }
  trait Analyzer extends Typers with Infer {
    val global: Global
  }
  class Global {
    type Type
    lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
    object typer extends analyzer.Typer
  }
}

object t21585Types {

  trait O1[O1_A] {
    type O1_A_Alias = O1_A
    trait I1 {
      def foo: O1_A_Alias
      def bar(p: O1_A_Alias): Unit = ()
    }
  }

  trait O2 extends O1[Int] {
    trait I2 extends I1 with O1[String] {
      def goodCodeRed = {
        var x = this.foo
        x = 1 // good code red
        this.bar(1) // good code red
      }

//      def badCodeGreen = {
//        var x = this.foo
//        x = "" // bad code green
//        this.bar("") // bad code green
//      }
    }
  }
}
