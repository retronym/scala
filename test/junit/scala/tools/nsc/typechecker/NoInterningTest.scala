package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

/** Compile with a `Global` whose `unique` interns nothing, so that every type it makes is a new
 *  instance. Each test is a program that was only accepted, or only rejected with the right error,
 *  because two equal types were the same instance.
 */
@RunWith(classOf[JUnit4])
class NoInterningTest {
  private def errors(code: String, stopAfter: String, options: String = ""): List[String] = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.processArgumentString(options)
    settings.stopAfter.value = List(stopAfter)
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter) {
      override protected def unique[T <: Type](tp: T): T = tp
    }
    new global.Run().compileSources(List(new BatchSourceFile("test.scala", code)))
    reporter.infos.toList.map(_.msg)
  }

  private def assertError(expected: String, actual: List[String]): Unit =
    assertTrue(actual.mkString("\n"), actual.exists(_.contains(expected)))

  // `hasSelfType` compared type constructors with `ne`, so `samOf` saw a self type here.
  @Test def samWithOwnSelfType(): Unit =
    assertEquals(Nil, errors("trait F[A] { self: F[A] => def apply(a: A): A }; object T { val f: F[Int] = x => x }", "typer"))

  // `SpecializeTypes` made the generic class the self type of the specialized one.
  @Test def specializedClassTag(): Unit =
    assertEquals(Nil, errors("class Foo[@specialized(Int) T: scala.reflect.ClassTag] { val a = new Array[T](1) }", "specialize"))

  // `singleTypeAsSeen` rebound `self` to the overriding val that fields adds to `C`, so the setter of `n`
  // no longer agreed with its field. See pos/trait_fields_dependent_rebind.scala.
  @Test def fieldsDependentOnTraitVal(): Unit =
    assertEquals(Nil, errors(
      """trait N { val self: N = ???; val n: self.type = self }
        |abstract class M { val self: N; val n: self.type }
        |class C extends M with N""".stripMargin, "fields"))

  // The subtyping cycle check in `validateRelativeInfo` compared with `eq`; see neg/t5093.scala.
  @Test def cyclicUpperBound(): Unit =
    assertError("illegal cyclic reference involving type C", errors("class T { def f[C[X] <: C[X]](l: C[_]) = l.x }", "typer"))

  // `AliasTypeRef.normalizeImpl` stopped only when `betaReduce` returned `this`; see neg/t10228.scala.
  @Test def cyclicAlias(): Unit =
    assertError("malformed type: A#Type", errors(
      """object t10228 {
        |  trait Data { self => type Type >: self.type <: Data }
        |  trait DataF[P <: Data, A <: DataF[P, A]] extends Data { type Type = P#Type }
        |  type Fili[A <: Data] = DataF[A, A]
        |  def g[A <: Fili[A]]: Unit = implicitly[A <:< A#Type]
        |}""".stripMargin, "typer"))

  // `firstChangedSymbol` took an equal but new bound for a change and cloned the type parameter, so the
  // implicit scope of `CoSet[U, Any]` met a new `CoSet[U, A1]` on every round; see neg/t6528.scala.
  @Test def cloneOnlyChangedTypeParams(): Unit =
    assertError("could not find implicit value", errors(
      """trait CoSet[U, +A <: U] extends CoSetLike[U, A, ({type S[A1 <: U] = CoSet[U, A1]})#S]
        |trait CoSetLike[U, +A <: U, +This[X] <: CoSetLike[U, A, This] with CoSet[U, A]] {
        |  implicitly[CoSet[U, Any]]
        |}""".stripMargin, "typer"))

  // `=:=` and `<:<` skip the `-explaintypes` trace only for `eq` types, so equal prefixes such as `c.type`
  // added steps to the trace; see neg/t6123-explaintypes-macros.
  @Test def explainTypesSkipsEqualTypes(): Unit = {
    val messages = errors(
      """import scala.language.experimental.macros
        |import scala.reflect.macros.blackbox.Context
        |object BadMac {
        |  def printf(format: String, params: Any*): Unit = macro printf_impl
        |  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[String]*): c.Expr[Unit] = ???
        |}""".stripMargin, "typer", "-explaintypes")
    assertEquals(List("c.universe.Expr[Any]* <: c.universe.Expr[String]*?", "false"), messages.init)
    assertError("macro implementation has incompatible shape", messages.takeRight(1))
  }
}
