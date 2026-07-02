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

  /** IntelliJ's anchored doUpdateThisTypeFromClass:
   *  Lockstep (t baseType clazz).prefix / clazz.owner until we land on thisSym. */
  private def anchoredMatch(target: Type, clazz: Symbol, thisSym: Symbol): Option[Type] = {
    if (thisSym == clazz) return Some(target)
    var t: Type   = target
    var c: Symbol = clazz
    while (c != NoSymbol && c.isClass) {
      val bt = t.baseType(c)
      if (bt == NoType) return None
      t = bt.prefix
      c = c.owner
      if (c.isClass && c == thisSym) return Some(t)
    }
    None
  }

  /** Apply fused chain: first matching update REPLACES the leaf, the remainder
   *  of the chain is applied to the replacement.  Non-matching nodes descend
   *  with the FULL chain (mirroring IntelliJ recursiveUpdateImpl). */
  private def applyFused(tp: Type, upds: List[FusedUpd]): Type = {
    def tryMatch(t: Type, us: List[FusedUpd]): Option[(Type, List[FusedUpd])] = us match {
      case Nil => None
      case u :: rest =>
        val hit: Option[Type] = u match {
          case FThisUpd(target, anchorOpt) => t match {
            case ThisType(sym) => anchorOpt match {
              case Some(clazz) => anchoredMatch(target, clazz, sym)
              case None        => anchorlessMatch(target, sym)
            }
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
      case Some((replacement, rest)) => applyFused(replacement, rest)
      case None =>
        val descend = new TypeMap { def apply(t: Type) = applyFused(t, upds) }
        descend.mapOver(tp)
    }
  }

  /** arm-1 (IntelliJ hasRecursiveThisType0 arm-1): block when the TARGET
   *  structurally contains the this-type being rewritten.  O(type-size). */
  private def arm1Block(target: Type, thisSym: Symbol): Boolean = {
    var found = false
    new TypeMap {
      def apply(t: Type): Type = {
        if (!found) t match {
          case ThisType(s) if s == thisSym => found = true
          case _ => mapOver(t)
        }
        t
      }
    }.apply(target)
    found
  }

  /** root-of-spine discriminator (candidate O(depth) alternative to arm-1):
   *  block only if thisSym IS the root of the target's prefix spine.
   *  "Spine root" = the innermost this-type on the path (stop at ThisType, not
   *  at NoType, because scalac's ThisType.prefix delegates to underlying.prefix
   *  and climbs into enclosing module types — past the path's logical root).
   *  Hypothesis: equivalent to arm-1 for path-shaped targets. */
  private def rootOfSpineBlock(target: Type, thisSym: Symbol): Boolean = {
    @annotation.tailrec
    def root(t: Type): Type = t match {
      case _: ThisType => t  // stop here: this IS the path root
      case _ => t.prefix match {
        case NoType | NoPrefix => t
        case p                 => root(p)
      }
    }
    root(target) match {
      case ThisType(s) => s == thisSym
      case _           => false
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
  // Fixture 1 — the self-embedding growth pump.
  //
  // Uses Analyzer.this as the root (analogous to IntelliJ's Infer.this pump):
  //   target₀ = Analyzer.this.global.analyzer.type
  //   anchorlessMatch(target₀, Analyzer) = Some(target₀)  [Analyzer ≤: Analyzer]
  //   Substitute Analyzer.this → target₀  in  memberType = Analyzer.this
  //   → result  = target₀   (whole target returned; self-embedding)
  //   → grow: select .global.analyzer from result → target₁  (2× deep)
  //   → repeat: spineDepth grows +2 per round without a discriminator.
  //
  // arm-1:        target₀ contains Analyzer.this → BLOCK → depth stays 0, fixpoint.
  // root-of-spine: root(target₀) = Analyzer.this = sym → BLOCK → same fixpoint.
  //
  // The two discriminators must agree on every round (equivalence conjecture for
  // path-shaped targets — the key claim to verify before backporting).
  // ─────────────────────────────────────────────────────────────────────────
  @Test def fusedSubstPump(): Unit = {
    import inferencerTypes._

    val analyzerThis = ThisType(symbolOf[Analyzer])
    val globalSym    = analyzerThis.member(TermName("global"))
    val globalPath   = singleType(analyzerThis, globalSym) // Analyzer.this.global.type
    val analyzerSym  = globalPath.member(TermName("analyzer"))

    // The type substituted each round: Analyzer.this (one leaf, depth change unambiguous).
    val memberType: Type = analyzerThis
    println(s"\n=== fusedSubstPump ===")
    println(s"analyzerSym = $analyzerSym  info = ${analyzerSym.info}")
    println(s"memberType  = $memberType  spineDepth = ${spineDepth(memberType)}")

    // Grow path P by appending .global.analyzer
    def growPath(p: Type): Type = {
      val gSym  = p.member(TermName("global"))
      val gPath = singleType(p, gSym)
      val aSym  = gPath.member(TermName("analyzer"))
      singleType(gPath, aSym)
    }

    // === No discriminator — pump should grow ===
    println("\n--- no discriminator (pump) ---")
    var targetND: Type = singleType(globalPath, analyzerSym)
    val depthsND = (1 to 5).map { round =>
      val result = applyFused(memberType, List(FThisUpd(targetND, None)))
      val d = spineDepth(result)
      println(s"  round $round: target=$targetND  result=$result  depth=$d")
      targetND = growPath(result)
      d
    }
    for (i <- 0 until depthsND.length - 1)
      assertTrue(s"pump should grow at round ${i+1}: ${depthsND(i)} vs ${depthsND(i+1)}",
        depthsND(i) < depthsND(i + 1))
    println(s"  ✓ growth confirmed: depths = ${depthsND.mkString(", ")}")

    // === arm-1 — should fixpoint ===
    println("\n--- arm-1 discriminator ---")
    var targetArm1: Type = singleType(globalPath, analyzerSym)
    val depthsArm1 = (1 to 5).map { round =>
      val blocked = arm1Block(targetArm1, symbolOf[Analyzer])
      val eff     = if (blocked) analyzerThis else targetArm1
      val result  = applyFused(memberType, List(FThisUpd(eff, None)))
      val d       = spineDepth(result)
      println(s"  round $round: blocked=$blocked  target=$targetArm1  result=$result  depth=$d")
      targetArm1 = growPath(result)
      d
    }
    assertEquals(s"arm-1 should fixpoint; depths=${depthsArm1.mkString(",")}", 1, depthsArm1.distinct.size)
    println(s"  ✓ arm-1 fixpoint at depth ${depthsArm1.head}")

    // === root-of-spine — should fixpoint ===
    println("\n--- root-of-spine discriminator ---")
    var targetROS: Type = singleType(globalPath, analyzerSym)
    val depthsROS = (1 to 5).map { round =>
      val blocked = rootOfSpineBlock(targetROS, symbolOf[Analyzer])
      val eff     = if (blocked) analyzerThis else targetROS
      val result  = applyFused(memberType, List(FThisUpd(eff, None)))
      val d       = spineDepth(result)
      println(s"  round $round: blocked=$blocked  target=$targetROS  result=$result  depth=$d")
      targetROS = growPath(result)
      d
    }
    assertEquals(s"root-of-spine should fixpoint; depths=${depthsROS.mkString(",")}", 1, depthsROS.distinct.size)
    println(s"  ✓ root-of-spine fixpoint at depth ${depthsROS.head}")

    // === Both discriminators must agree on every round ===
    assertEquals("arm-1 ≡ root-of-spine for path-shaped targets", depthsArm1, depthsROS)
    println(s"  ✓ arm-1 ≡ root-of-spine confirmed for all 5 rounds")
  }
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
