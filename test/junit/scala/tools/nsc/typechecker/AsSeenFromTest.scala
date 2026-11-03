package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertSame, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

/** `tp.asSeenFrom(C.this, C)` returns `tp` without running `AsSeenFromMap` when that is the identity. */
class AsSeenFromTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  val code =
    """package p
      |import scala.annotation.unchecked.uncheckedVariance
      |trait Outer[O] {
      |  class Inner[I] { def o: O = ???; def i: I = ???; def both: (O, I) = ???; def out: Outer.this.type = ??? }
      |}
      |trait Self { self: Outer[Int] => class InSelf[I] { def i: List[I] = ??? } }
      |class SelfInt[T] { self: SelfInt[Int] => def f: T = ??? }
      |class Ann[+T] { def v: List[T @uncheckedVariance] = ??? }
      |""".stripMargin

  lazy val g = {
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    compiler.global
  }
  import g._

  def mapped(tp: Type, pre: Type, clazz: Symbol): Type = {
    val m = new AsSeenFromMap(pre, clazz)
    existentialAbstraction(m.capturedParams, m(tp))
  }

  def cls(path: String): Symbol = {
    val top :: nested = path.split('.').toList.drop(1): @unchecked
    nested.foldLeft(rootMirror.getRequiredClass("p." + top): Symbol)((c, n) => c.info.decl(TypeName(n)))
  }

  def ownThis(clsName: String, memberName: String): (Type, Type, Type) = exitingTyper {
    val cls = this.cls(clsName)
    val tp = cls.info.decl(TermName(memberName)).info
    (tp, tp.asSeenFrom(cls.thisType, cls), mapped(tp, cls.thisType, cls))
  }

  @Test def identity(): Unit = {
    for ((cls, m) <- List("p.Outer.Inner" -> "o", "p.Outer.Inner" -> "i", "p.Outer.Inner" -> "both", "p.Outer.Inner" -> "out", "p.Self.InSelf" -> "i")) {
      val (tp, fast, slow) = ownThis(cls, m)
      assertSame(s"$cls.$m", slow, tp)
      assertSame(s"$cls.$m", tp, fast)
    }
  }

  @Test def selfTypeRefinesClass(): Unit = {
    val (tp, fast, slow) = ownThis("p.SelfInt", "f")
    assertEquals("Int", slow.resultType.toString)
    assertEquals(slow.toString, fast.toString)
    assertTrue(tp ne fast)
  }

  @Test def annotationDropped(): Unit = {
    val (tp, fast, slow) = ownThis("p.Ann", "v")
    assertEquals("List[T]", slow.resultType.toString)
    assertEquals(slow.toString, fast.toString)
    assertTrue(tp ne fast)
  }

  @Test def typeVarInstantiated(): Unit = exitingTyper {
    val cls = this.cls("p.Outer.Inner")
    val tv = TypeVar(cls.typeParams.head)
    tv.setInst(definitions.IntTpe)
    val tp = typeRef(NoPrefix, definitions.ListClass, List(tv))
    val fast = tp.asSeenFrom(cls.thisType, cls)
    assertEquals("List[Int]", fast.toString)
    assertEquals(mapped(tp, cls.thisType, cls).toString, fast.toString)
  }
}
