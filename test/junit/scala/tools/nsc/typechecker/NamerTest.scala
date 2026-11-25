package scala.tools.nsc.typechecker

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class NamerTest extends BytecodeTesting {

  import compiler.global._

  override def compilerArgs: String = "-Ystop-after:typer"

  @Test
  def defaultMethodsInDeclarationOrder(): Unit = {
    compiler.compileClasses("package p1; class Test { C.b(); C.a() }; object C { def a(x: Int = 0) = 0; def b(x: Int = 0) = 0 }")
    val methods = compiler.global.rootMirror.getRequiredModule("p1.C").info.decls.toList.map(_.name.toString).filter(_.matches("""(a|b).*"""))
    def getterName(s: String) = nme.defaultGetterName(TermName(s), 1).toString
    Assert.assertEquals(List("a", getterName("a"), "b", getterName("b")), methods) // order no longer depends on order of lazy type completion :)
  }

  // Members without type or value parameters are completed in a context that shares the
  // enclosing scope. Nothing their signatures type may be entered into that scope.
  @Test
  def memberSignaturesEnterNothingIntoEnclosingScope(): Unit = {
    val code =
      """package p2
        |class C {
        |  implicit val ord: Ordering[Int] = Ordering.Int
        |  def named(a: Int = 1, b: Int = 2) = a + b
        |  val existential: Class[_] = classOf[String]
        |  val existential2: List[T] forSome { type T <: AnyRef } = Nil
        |  val refinement: AnyRef { def foo: Int } = new { def foo = 1 }
        |  val anon = new Runnable { def run(): Unit = () }
        |  val fun = (x: Int) => { val y = x; y }
        |  val matched = (1: Any) match { case i: Int => i; case s: String => s.length }
        |  val namedArgs = named(b = existential.getName.length)
        |  val viaImplicit = implicitly[Ordering[Int]]
        |  var v = { def local = 1; local }
        |  lazy val lz = { class Local; new Local }
        |  def parameterless = List(1).map { case x if x > 0 => x }
        |  def emptyParams() = { val z = 1; z }
        |  type Alias = Map[_, _ <: AnyRef]
        |  type Bounded >: Null <: Option[_]
        |  def poly[A[_] <: Iterable[_], B](a: A[B], f: B => B = (b: B) => b): A[B] = a
        |}
        |""".stripMargin
    compiler.compileClasses(code)
    val clazz = compiler.global.rootMirror.getRequiredClass("p2.C")
    val decls = clazz.info.decls.toList
    Assert.assertEquals(Nil, decls.filterNot(_.owner == clazz))
    val names = decls.map(_.name.decoded).filterNot(n => n.contains("$default$") || n.endsWith(" ")).distinct.sorted
    val expected = List("<init>", "Alias", "Bounded", "anon", "emptyParams", "existential", "existential2", "fun", "lz",
      "matched", "named", "namedArgs", "ord", "parameterless", "poly", "refinement", "v", "v_=", "viaImplicit").sorted
    Assert.assertEquals(expected, names)
  }
}
