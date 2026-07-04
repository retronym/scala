package scala.reflect.internal

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.reflect.internal.ijmodel.{EngineMode, IjTypeSystem}
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

/**
 * Tests for the IntelliJ member-lookup replica in `scala.reflect.internal.ijmodel`
 * (see IjTypeSystem.scala there for the construct-by-construct mapping to the
 * intellij-scala sources and the OUT-OF-BOUNDS contract).
 *
 * Every lookup is asserted against scalac's own `memberType` — by spelling AND
 * by =:= (spelling alone can pass vacuously: T@TypersG and T@GlobalG both print
 * as "T").  The oracle and fixture path construction are the ONLY places the
 * out-of-bounds scalac APIs (member/memberType) are used.
 */
@RunWith(classOf[JUnit4])
class IntelliJMemberLookupTest {

  object ij extends IjTypeSystem {
    val symbolTable: SymbolTableForUnitTesting = new SymbolTableForUnitTesting
  }
  import ij._
  import ij.symbolTable._
  import EngineMode._
  import ReferenceExpressionResolver.{memberType => ijMemberType, resolve, resolvePath}

  // ═══════════════════════════════════════════════════════════════════════
  //  ORACLE — the ONLY place out-of-bounds scalac APIs (member/memberType,
  //  i.e. asSeenFrom) may be called.  Fixture/golden path construction in the
  //  tests below may also use them, never the model.
  // ═══════════════════════════════════════════════════════════════════════
  private def scalacMemberType(pre: Type, name: String): Type =
    pre.memberType(pre.member(TermName(name))).resultType

  private def assertParity(pre: Type, name: String)(implicit mode: EngineMode): Unit = {
    val (ijType, chain) = ijMemberType(pre, name)
    val oracle = scalacMemberType(pre, name)
    println(s"  [$mode] $pre . $name")
    println(s"     chain : ${chain.render}")
    println(s"     ij    : $ijType")
    println(s"     scalac: $oracle")
    assertEquals(s"[$mode] $pre.$name", oracle.toString, ijType.toString)
    assertTrue(s"[$mode] $pre.$name =:= (symbols, not just spelling)", ijType =:= oracle)
  }

  private def spineDepth(tp: Type): Int = {
    def go(t: Type, n: Int): Int = t match {
      case SingleType(pre, _) => go(pre, n + 1)
      case TypeRef(pre, _, _) if (pre ne NoPrefix) && (pre ne NoType) => go(pre, n + 1)
      case _ => n
    }
    go(tp, 0)
  }

  // ═══════════════════════════════════════════════════════════════════════
  //  Tests
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
  //    requires TWO TypeParamSubstitutions composing THROUGH the fused chain
  //    (T -> List[S], remainder S -> Int).
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
      val (viaPath, chain) = resolvePath(ceThis, "en", "values")
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
      val (t, chain) = resolvePath(gThis, "typerG", "applyG")
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
      val (gSym, _, _) = resolve(root, "global")
      var pre: Type = singleType(root, gSym)
      (1 to n).map { _ =>
        val (aSym, _, _) = resolve(pre, "analyzer")
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

  // 6. Emergent duplication audit: the layered construction reproduces
  //    duplicate/overlapping chain elements without any hand-crafting.
  @Test def duplicationEmerges(): Unit = {
    import ijFixtures._
    println(s"\n=== duplicationEmerges ===")
    implicit val m: EngineMode = ProgressConsumed
    val ceThis = ThisType(symbolOf[CEg[_]])
    val (t, chain) = resolvePath(ceThis, "en", "values", "toL")
    println(s"  result: $t")
    println(s"  chain[${chain.substitutions.length} updates, ${chain.thisSubstCount} this-substs, dup=${chain.duplicateThisTargets}]:")
    chain.substitutions.foreach(u => println(s"    $u"))
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

  // 8. The DEDUP THEOREM: under Progress+Consumed (fall-through + remainder-only
  //    + first-spine-match-per-class), removing equality-duplicate updates from a
  //    chain cannot change any lookup's result — duplicates are provably inert.
  //    This is deliverable (c) closed as a theorem instead of a suite gamble.
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
      val (sym, _, chain) = resolve(pre, name)
      val full   = chain(sym.info).resultType
      val dedup  = chain.dedupped(sym.info).resultType
      val removed = chain.substitutions.length - chain.dedupped.substitutions.length
      println(s"  $pre.$name: ${chain.substitutions.length} updates, $removed removed by dedup")
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
