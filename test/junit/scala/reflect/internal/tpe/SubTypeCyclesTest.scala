package scala.reflect.internal
package tpe

import org.junit.Assert._
import org.junit.Test

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object SubTypeCyclesTest {
  final class TooManyCalls(limit: Long) extends AssertionError(s"more than $limit calls of isSubType")

  /** A compiler that counts the calls of `isSubType` and gives up after `limit` of them. */
  final class Counting {
    private val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.processArgumentString("-usejavacp -Ystop-after:typer -nowarn")
    private val reporter = new StoreReporter(settings)

    var calls = 0L
    var limit = Long.MaxValue
    /** Has a check cut off a pair as a cycle? */
    var cutOff = false

    val global: Global = new Global(settings, reporter) {
      override def isSubType(tp1: Type, tp2: Type, depth: Depth): Boolean = {
        calls += 1
        if (calls > limit) throw new TooManyCalls(limit)
        try super.isSubType(tp1, tp2, depth)
        finally if (subTypeCycles.cutOff) cutOff = true
      }
    }

    /** The calls of `isSubType` made to type check `code`. */
    def typecheck(code: String, limit: Long = 5000000L): Long = {
      reporter.reset()
      calls = 0; cutOff = false; this.limit = limit
      var thrown: Throwable = null
      // typing a long `::` type needs more stack than the test runner has
      val thread = new Thread(null, () => {
        try new global.Run().compileSources(List(new BatchSourceFile("test.scala", code)))
        catch { case t: Throwable => thrown = t }
      }, "typecheck", 256L * 1024 * 1024)
      thread.start(); thread.join()
      this.limit = Long.MaxValue
      if (thrown ne null) throw thrown
      assertFalse(reporter.infos.mkString("\n"), reporter.hasErrors)
      calls
    }
  }

  lazy val compiler = new Counting

  val hierarchy =
    """trait N[-A]
      |trait Y extends N[Any]
      |object H { type NDY = N[D with Y] }
      |trait D extends N[H.NDY]
      |trait X
      |""".stripMargin

  val hlist =
    """trait HList
      |final case class ::[+H, +T <: HList](h: H, t: T) extends HList
      |sealed trait HNil extends HList
      |""".stripMargin

  def nest(depth: Int, elems: String*): String = List.tabulate(depth)(i => elems(i % elems.length)).mkString("", " :: ", " :: HNil")
}

class SubTypeCyclesTest {
  import SubTypeCyclesTest._

  // Conformance of two long `::` types recurses far past the depth from which pairs are recorded, but finds no
  // pair pending. It must be left alone: not taken for a cycle, and linear in the depth.
  @Test def deepTypesAreNotCycles(): Unit = {
    def calls(depth: Int) = {
      val n = compiler.typecheck(
        s"""$hlist
           |object T {
           |  type L1 = ${nest(depth, "Int", "String")}
           |  type L2 = ${nest(depth, "AnyVal", "CharSequence")}
           |  def f(x: L1): L2 = x
           |  def g(x: List[L1]): List[L2] = x
           |}""".stripMargin)
      assertFalse(compiler.cutOff)
      n
    }
    val small = calls(200)
    val large = calls(400)
    assertTrue(s"$small calls at depth 200, $large at 400", large < small * 5 / 2)
  }

  // The same types beside a pair that is cut off as a cycle, in one check.
  @Test def deepTypesBesideACycle(): Unit = {
    def calls(depth: Int) = {
      val n = compiler.typecheck(
        s"""$hlist
           |$hierarchy
           |object T {
           |  type L1 = ${nest(depth, "Int", "String")}
           |  type L2 = ${nest(depth, "AnyVal", "CharSequence")}
           |  def f(x: (D, L1, D)): (H.NDY, L2, H.NDY) = x
           |  def g(x: (D, List[L1])): (H.NDY, List[L2]) = x
           |}""".stripMargin)
      assertTrue(compiler.cutOff)
      n
    }
    val small = calls(200)
    val large = calls(400)
    assertTrue(s"$small calls at depth 200, $large at 400", large < small * 5 / 2)
  }

  // scala/bug#13119, with `n` components in the intersection. Without help the calls double with each component.
  @Test def t13119(): Unit = {
    def calls(n: Int) = {
      val ts = List.tabulate(n)(i => s"T$i")
      compiler.typecheck(
        s"""trait A[T]
           |final class C[${ts.mkString(", ")}] extends A[${ts.mkString(" with ")}]
           |object T {
           |  def f(x: A[_]): Int = x match {
           |    case b: C[${ts.map(_ => "_").mkString(", ")}] => 0
           |    case _ => 1
           |  }
           |}""".stripMargin)
    }
    val two = calls(2)
    val eight = calls(8)
    assertTrue(s"$two calls for 2 components, $eight for 8", eight < two * 30)
  }

  // Each turn of the loop asks four questions; each of them has to be answered once.
  @Test def twoContravariantParameters(): Unit = {
    def calls(n: Int) = {
      val ds = List.tabulate(n)(i => s"D$i")
      val all = (ds :+ "Y").mkString(" with ")
      val arg = s"N[$all, $all]"
      compiler.typecheck(
        s"""trait N[-A, -B]
           |trait Y extends N[Any, Any]
           |${ds.map(d => s"trait $d extends N[$arg, $arg]").mkString("\n")}
           |object T { def f(d: D0): $arg = d }
           |""".stripMargin, limit = 100000)
    }
    val three = calls(3)
    val nine = calls(9)
    assertTrue(s"$three calls for 3 traits, $nine for 9", nine < three * 30)
  }

  // A pair that conforms by adding a bound to a type variable is not remembered: `S <: X` holds by `?T <: X`
  // for `S <: ?T`, although neither `S` nor `X` contains a type variable. The first parent of the intersection
  // derives it and fails on its last component, which drops the bound. The second parent has to add it again.
  @Test def boundOfTypeVariableIsNotLost(): Unit = {
    compiler.typecheck(hierarchy)
    val global: compiler.global.type = compiler.global
    import global._, definitions._
    exitingTyper {
      val d = rootMirror.staticClass("D")
      val ndy = rootMirror.staticModule("H").info.member(TypeName("NDY")).tpe_*
      val x = rootMirror.staticClass("X").tpe_*
      val tvar = TypeVar(d.newTypeParameter(TypeName("T")).setInfo(TypeBounds.empty))
      val s = d.newAbstractType(TypeName("S")).setInfo(TypeBounds.upper(tvar)).tpe_*
      val lhs = intersectionType(List(tupleType(List(d.tpe_*, s, StringTpe)), tupleType(List(d.tpe_*, s, IntTpe))))
      val rhs = tupleType(List(ndy, x, IntTpe))
      assertTrue(lhs <:< rhs)
      assertEquals(List(x), tvar.constr.hiBounds)
    }
  }

  @Test def generatedHierarchies(): Unit =
    for (family <- SubTypeCyclesCases.families) {
      val pairs = family.pairs.linesIterator.filter(_.nonEmpty).map { line =>
        val Array(answer, pair) = line.takeWhile(_ != '/').split('|').map(_.trim)
        val Array(lhs, rhs) = pair.split(" <:< ")
        (lhs, rhs, answer.toBoolean)
      }.toList
      val aliases = pairs.zipWithIndex.map { case ((lhs, rhs, _), i) => s"  type L$i = $lhs\n  type R$i = $rhs\n" }
      compiler.typecheck(SubTypeCyclesCases.common + family.definitions + aliases.mkString("\nobject Q {\n", "", "}\n"))
      val global: compiler.global.type = compiler.global
      import global._
      exitingTyper {
        val q = rootMirror.staticModule("Q").info
        for (((lhs, rhs, answer), i) <- pairs.zipWithIndex) {
          compiler.calls = 0; compiler.limit = 100000
          val conforms = try q.member(TypeName(s"L$i")).info <:< q.member(TypeName(s"R$i")).info finally compiler.limit = Long.MaxValue
          assertEquals(s"$lhs <:< $rhs with\n${family.definitions}", answer, conforms)
        }
      }
    }
}
