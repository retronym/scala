package scala.tools.nsc
package typechecker

import org.junit.Assert._
import org.junit.Test

import RefchecksAfterErrorsHarness._

/** Probes for `-Yrefchecks-after-errors` (scala/scala#11030).
 *
 *  Three groups:
 *   - `value_*`: diagnostics that the flag is supposed to unlock; must appear.
 *   - `noise_*`: refchecks diagnostics that are only a *consequence* of the typer error (ErrorType / missing parent /
 *     failed inference leaking into override or abstractness checks). `extra` must be empty. A failure is a finding.
 *   - `crash_*`: refchecks must not throw on half-typed trees.
 */
class RefchecksAfterErrorsTest {
  private def noise(code: String): Unit = {
    val c = compare(code)
    assertTrue(s"baseline should contain a typer error:\n${c.render}", c.baseline.errors.nonEmpty)
    assertEquals(s"spurious extra diagnostics:\n${c.render}", Nil, c.extra)
  }
  private def valueAdded(code: String, expected: String): Unit = {
    val c = compare(code)
    assertTrue(s"baseline should contain a typer error:\n${c.render}", c.baseline.errors.nonEmpty)
    assertNull(s"crash: ${c.withFlag.crash}", c.withFlag.crash.orNull)
    assertTrue(s"expected an extra diagnostic containing '$expected':\n${c.render}", c.extra.exists(_.msg.contains(expected)))
  }
  private def noCrash(code: String): Unit = {
    val c = compare(code)
    c.withFlag.crash.foreach { t => t.printStackTrace(); fail(s"crash with flag: $t\n${c.render}") }
  }

  // ---------------------------------------------------------------- the point of the PR

  @Test def value_missingImplementation(): Unit = valueAdded(
    """trait T { def f: Int }
      |class C extends T { val x: Int = "" }
      |""".stripMargin, "needs to be abstract")

  @Test def value_overridesNothing(): Unit = valueAdded(
    """class C { override def nope: Int = 1; val x: Int = "" }
      |""".stripMargin, "overrides nothing")

  @Test def value_finalOverride(): Unit = valueAdded(
    """class A { final def f = 1 }
      |class B extends A { override def f = 2; val x: Int = "" }
      |""".stripMargin, "cannot override final member")

  @Test def value_incompatibleOverride(): Unit = valueAdded(
    """class A { def f: Int = 1 }
      |class B extends A { override def f: String = ""; val x: Int = "" }
      |""".stripMargin, "incompatible type")

  @Test def value_pureExpressionWarning(): Unit = valueAdded(
    """class C { def f = { 1; 2 }; val x: Int = "" }
      |""".stripMargin, "pure expression")

  // ---------------------------------------------------------------- ErrorType noise: override / implementation

  /** Inferred result type of the implementing method is ErrorType. */
  @Test def noise_inferredErrorTypeResult(): Unit = noise(
    """trait T { def f: Int }
      |class C extends T { def f = undefinedName }
      |""".stripMargin)

  @Test def noise_inferredErrorTypeResult_override(): Unit = noise(
    """trait T { def f: Int }
      |class C extends T { override def f = undefinedName }
      |""".stripMargin)

  /** Parameter type is unresolved => member no longer matches the parent's, so "overrides nothing". */
  @Test def noise_overridesNothing_errorParamType(): Unit = noise(
    """trait T { def f(x: Int): Int }
      |class C extends T { override def f(x: Undefined): Int = 1 }
      |""".stripMargin)

  /** Same, without `override`: reported as missing implementation. */
  @Test def noise_missingImpl_errorParamType(): Unit = noise(
    """trait T { def f(x: Int): Int }
      |class C extends T { def f(x: Undefined): Int = 1 }
      |""".stripMargin)

  /** Unresolved parent contributes the members; `override` on them looks bogus. */
  @Test def noise_overridesNothing_missingParent(): Unit = noise(
    """class C extends Missing { override def run(): Unit = () }
      |""".stripMargin)

  @Test def noise_overridesNothing_anonClassMissingParent(): Unit = noise(
    """object O { val x = new Missing { override def run(): Unit = () } }
      |""".stripMargin)

  @Test def noise_overridesNothing_objectMissingParent(): Unit = noise(
    """object O extends Missing { override def toString(): String = "" ; override def zzz = 1 }
      |""".stripMargin)

  /** Missing parent + a good abstract parent: members the missing parent would have supplied are "missing". */
  @Test def noise_missingImpl_missingParentWouldProvide(): Unit = noise(
    """trait T { def f: Int }
      |class C extends Missing with T
      |""".stripMargin)

  @Test def noise_missingImpl_typeArgIsError(): Unit = noise(
    """class C extends Ordering[Undefined] { }
      |""".stripMargin)

  @Test def noise_missingImpl_typeArgIsError_impl(): Unit = noise(
    """class C extends Ordering[Undefined] { def compare(x: Undefined, y: Undefined): Int = 0 }
      |""".stripMargin)

  @Test def noise_missingImpl_anonClassTypeArgError(): Unit = noise(
    """object O { val o = new Ordering[Undefined] { def compare(x: Undefined, y: Undefined) = 0 } }
      |""".stripMargin)

  /** Abstract member whose own result type is error. */
  @Test def noise_overrideErrorTypeParent(): Unit = noise(
    """trait T { def f: Undefined }
      |class C extends T { def f: Int = 1 }
      |""".stripMargin)

  @Test def noise_overrideErrorTypeParent_valVsDef(): Unit = noise(
    """trait T { def f: Undefined }
      |class C extends T { val f = 1 }
      |""".stripMargin)

  @Test def noise_abstractTypeBoundError(): Unit = noise(
    """trait A { type T <: Undefined }
      |class B extends A { type T = Int }
      |""".stripMargin)

  @Test def noise_aliasToError(): Unit = noise(
    """trait A { type T <: AnyRef }
      |class B extends A { type X = Undefined; override type T = X }
      |""".stripMargin)

  @Test def noise_incompatibleReturn_inferredFromBadCall(): Unit = noise(
    """class A { def f: Int = 1 }
      |class B extends A { override def f = "".nope() }
      |""".stripMargin)

  /** Overloaded member with an error-typed alternative and defaults. */
  @Test def noise_overloadedDefaults(): Unit = noise(
    """class A {
      |  def f(a: Int = 1) = a
      |  def f(b: Undefined) = 2
      |}
      |""".stripMargin)

  @Test def noise_differentTypeInstances(): Unit = noise(
    """trait T[X]
      |class C extends T[Undefined] with T[Int]
      |""".stripMargin)

  @Test def noise_differentTypeInstances_inferred(): Unit = noise(
    """trait T[X]
      |trait U extends T[Int]
      |class C extends U with T[Undefined]
      |""".stripMargin)

  @Test def noise_variance_errorInPosition(): Unit = noise(
    """class C[+A] { def f(x: Undefined[A]): Int = 1 }
      |""".stripMargin)

  @Test def noise_variance_inferredFromError(): Unit = noise(
    """class C[+A] { def f = undefinedName; def g(x: A) = f }
      |""".stripMargin.replace("def g(x: A) = f", "def g = f"))

  @Test def noise_caseClassMissingFieldType(): Unit = noise(
    """case class P(a: Undefined, b: Int)
      |class Q extends P(null, 1)
      |""".stripMargin)

  @Test def noise_caseClassParentOverride(): Unit = noise(
    """case class P(a: Int)
      |case class Q(a: Undefined) extends P(1)
      |""".stripMargin)

  @Test def noise_pureStatement_errorType(): Unit = noise(
    """class C { def f = { val x = undefinedName; x; 1 } }
      |""".stripMargin)

  @Test def noise_pureStatement_errorTypeSelect(): Unit = noise(
    """class C { def f = { this.nope; 1 } }
      |""".stripMargin)

  @Test def noise_deprecated_missing(): Unit = noise(
    """class C { @deprecated("x", "1") def d = 1; def f = d + undefinedName }
      |""".stripMargin.replace("def f = d + undefinedName", "def f = undefinedName"))

  @Test def noise_patternNameShadow(): Unit = noise(
    """class C { val Foo = 1; def f(x: Int) = x match { case Foo => 1; case y: Undefined => 2 } }
      |""".stripMargin)

  @Test def noise_recursiveCallSelfRef(): Unit = noise(
    """class C { def f: Int = this.f; val x: Int = "" }
      |""".stripMargin.replace("this.f", "undefinedName"))

  /** Super call to abstract member: checkSuper assertion is silenced by the PR. */
  @Test def noise_superAbstract(): Unit = noise(
    """trait A { def f: Int }
      |trait B extends A { override def f = super.f + undefinedName }
      |""".stripMargin)

  @Test def noise_selfTypeMissing(): Unit = noise(
    """trait T { def f: Int }
      |trait U { self: T with Missing => def g = f }
      |class C extends U
      |""".stripMargin)

  @Test def noise_mixinMissingSelfType(): Unit = noise(
    """trait T { self: Missing => def f = 1 }
      |object O extends T
      |""".stripMargin)

  @Test def noise_implicitClassPrivate(): Unit = noise(
    """class C { private implicit class I(x: Undefined) { def y = 1 } }
      |""".stripMargin)

  @Test def noise_enumLike(): Unit = noise(
    """abstract class E { def v: Int }
      |object A extends E { def v = Undef.v }
      |""".stripMargin)

  @Test def noise_javaStyleLambda(): Unit = noise(
    """class C { val r: Runnable = () => undefinedName; val c: java.util.Comparator[Int] = (a, b) => a.nope }
      |""".stripMargin)

  @Test def noise_lazyValOverride(): Unit = noise(
    """trait A { def x: Int }
      |class B extends A { lazy val x = undefinedName }
      |""".stripMargin)

  @Test def noise_valOverrideDef_mutable(): Unit = noise(
    """trait A { def x: Int }
      |class B extends A { var x = undefinedName }
      |""".stripMargin)

  @Test def noise_abstractClassNew(): Unit = noise(
    """abstract class A { def f(x: Int): Int }
      |object O { val a = new A { def f(x: Int): Undefined = 1 } }
      |""".stripMargin)

  @Test def noise_cyclicInheritance(): Unit = noise(
    """class A extends B
      |class B extends A
      |""".stripMargin)

  @Test def noise_cyclicAlias(): Unit = noise(
    """trait A { type T <: T }
      |object O extends A
      |""".stripMargin)

  @Test def noise_referenceToSealedBroken(): Unit = noise(
    """sealed trait S
      |case class A(x: Undefined) extends S
      |object O { def f(s: S) = s match { case A(_) => 1 } }
      |""".stripMargin)

  // ---- erroneous parent seen through an ancestor (Namers.checkParent replaced it by AnyRef in the ancestor's info)

  @Test def noise_transitive_overridesNothing(): Unit = noise(
    """class A extends Missing
      |class B extends A { override def run(): Unit = () }
      |""".stripMargin)

  @Test def noise_transitive_missingImpl(): Unit = noise(
    """trait T { def f: Int }
      |class A extends Missing with T
      |class B extends A
      |""".stripMargin)

  @Test def noise_transitive_viaTrait(): Unit = noise(
    """trait A extends Missing
      |object B extends A { override def run(): Unit = () }
      |""".stripMargin)

  @Test def noise_parentIsAliasToMissing(): Unit = noise(
    """class P { type X = Missing }
      |class C extends P#X { override def run(): Unit = () }
      |""".stripMargin)

  @Test def noise_parentTypeAliasErroneous(): Unit = noise(
    """object O { type P = Missing; class C extends P { override def run(): Unit = () } }
      |""".stripMargin)

  /** neg/t5529: the PR's own .check records "class Dir needs to be abstract. Missing implementation: def getClass()" as expected output. */
  @Test def noise_classTypeRequired_t5529(): Unit = noise(
    """object Test {
      |  sealed abstract class File { val i = 1 }
      |  sealed class Dir extends File { }
      |  type File
      |}
      |""".stripMargin)

  @Test def noise_classTypeRequired_abstractTypeMember(): Unit = noise(
    """class C { type T; class D extends T { override def foo = 1 } }
      |""".stripMargin)

  /** neg/reify_metalevel_breach_*: a failed macro expansion leaves `@compileTimeOnly` splice calls behind; refchecks then
   *  reports "splice must be enclosed within a reify {} block". The PR edited these tests to dodge the cascade. */
  @Test def noise_failedMacroLeavesCompileTimeOnly(): Unit = noise(
    """import scala.reflect.runtime.universe._
      |object Test {
      |  val code = reify {
      |    val x = 2
      |    val inner = reify { reify { x } }
      |    inner.splice.splice
      |  }
      |}
      |""".stripMargin)

  @Test def noise_javaParentErrorTypeArg(): Unit = noise(
    """class C extends java.util.Comparator[Undefined]
      |""".stripMargin)

  @Test def noise_ctorParamOverrideErrorType(): Unit = noise(
    """class A(val x: Int)
      |class B(override val x: Undefined) extends A(1)
      |""".stripMargin)

  @Test def noise_typeMemberErrorImpl(): Unit = noise(
    """trait A { type T }
      |class B extends A { override type T = List[Undefined] }
      |""".stripMargin)

  @Test def noise_polyMethodErrorBound(): Unit = noise(
    """trait T { def f[X <: Int](x: X): Int }
      |class C extends T { def f[X <: Undefined](x: X): Int = 1 }
      |""".stripMargin)

  @Test def noise_hkErrorArg(): Unit = noise(
    """trait T { def f[F[_]](x: F[Int]): Int }
      |class C extends T { def f[F[_]](x: F[Undefined]): Int = 1 }
      |""".stripMargin)

  @Test def noise_implicitParamErrorType(): Unit = noise(
    """trait T { def f(implicit x: Int): Int }
      |class C extends T { def f(implicit x: Undefined): Int = 1 }
      |""".stripMargin)

  @Test def noise_varargsErrorType(): Unit = noise(
    """trait T { def f(x: Int*): Int }
      |class C extends T { override def f(x: Undefined*): Int = 1 }
      |""".stripMargin)

  @Test def noise_byNameErrorType(): Unit = noise(
    """trait T { def f(x: => Int): Int }
      |class C extends T { override def f(x: => Undefined): Int = 1 }
      |""".stripMargin)

  @Test def noise_defaultGetterOverride(): Unit = noise(
    """class A { def f(x: Int = undefinedName) = x }
      |class B extends A { override def f(x: Int = 2) = x }
      |""".stripMargin)

  @Test def noise_valueClass(): Unit = noise(
    """class V(val x: Undefined) extends AnyVal
      |""".stripMargin)

  @Test def noise_valuePatternDef(): Unit = noise(
    """class C { val (a, b) = undefinedName; val Missing(c) = 1; def f = a + c }
      |""".stripMargin)

  // lints that look at the (error) types of expressions
  @Test def noise_sensibleComparison(): Unit = noise(
    """class C { def f(a: Int) = a == undefinedName; def g(s: String) = s == (null: Undefined) }
      |""".stripMargin)

  @Test def noise_inferAny(): Unit = noise(
    """class C { val x = List(1, undefinedName); val y = List(1, "")  .contains(undefinedName) }
      |""".stripMargin.replace("""  .contains(undefinedName)""", ""))

  @Test def noise_valueDiscard(): Unit = {
    val code = "class C { def f(): Unit = undefinedName; def g(x: Int) = { undefinedName(x); 1 } }\n"
    val c = compare(code, List("-Wvalue-discard", "-Wnonunit-statement"))
    assertEquals(s"spurious:\n${c.render}", Nil, c.extra)
  }

  @Test def noise_unusedValue(): Unit = {
    val code = "class C { def g(x: Int) = { x.nope; x.nope(); 1 } }\n"
    val c = compare(code, List("-Wnonunit-statement", "-Wunused"))
    assertEquals(s"spurious:\n${c.render}", Nil, c.extra)
  }

  @Test def noise_asInstanceOfError(): Unit = noise(
    """class C { def f(x: Any) = x.asInstanceOf[Undefined].foo; def g(x: Any) = x.isInstanceOf[List[Undefined]] }
      |""".stripMargin)

  // ---------------------------------------------------------------- crash corpus on half-typed trees

  @Test def crash_extractorMissing(): Unit = noCrash(
    """object O { def f(x: Any) = x match { case Missing(a, b) => a; case _ => 0 } }
      |""".stripMargin)

  @Test def crash_badNamedArgs(): Unit = noCrash(
    """class A { def f(a: Int, b: Int) = 1; f(a = 1, a = 2); f(c = 1) }
      |""".stripMargin)

  @Test def crash_badTypeApply(): Unit = noCrash(
    """class A { def f[T <: String](t: T) = t; f[Int](1); f[Undefined](2) }
      |""".stripMargin)

  @Test def crash_badNew(): Unit = noCrash(
    """class A { def f = new Undefined(1); def g = new A(1, 2) }
      |""".stripMargin)

  @Test def crash_badMacro(): Unit = noCrash(
    """import scala.language.experimental.macros
      |object M { def m(x: Int): Int = macro Undefined.impl; m(1) }
      |""".stripMargin)

  @Test def crash_forComprehensionBroken(): Unit = noCrash(
    """class A { def f = for { (a, b) <- List(1) ; c <- undefinedName } yield a }
      |""".stripMargin)

  @Test def crash_annotationBroken(): Unit = noCrash(
    """@throws[Undefined] class A { @Undefined def f = 1; @throws(classOf[Missing]) def g = 2 }
      |""".stripMargin)

  @Test def crash_existentialBroken(): Unit = noCrash(
    """class A { def f: List[_ <: Undefined] = Nil; def g: Undefined[_] = null }
      |""".stripMargin)

  @Test def crash_selfRefTypeCycle(): Unit = noCrash(
    """class A { val x: B = null; type B = A#C; type C = B }
      |""".stripMargin)

  @Test def crash_valDefMissingTpt(): Unit = noCrash(
    """class A { val x = y; val y = x; def f(a: Int = f()) = a }
      |""".stripMargin)

  @Test def crash_t510Like(): Unit = noCrash(
    """class A { type T <: B#T; class B { type T <: A#T } }
      |""".stripMargin)

  // ---------------------------------------------------------------- driver behaviour

  /** typerReportedErrors must not leak from one run to the next on a reused Global (IDE / sbt server). */
  @Test def stateDoesNotLeakAcrossRuns(): Unit = {
    val (g, r) = newGlobal(defaultArgs :+ Flag)
    runOn(g, r, List("a.scala" -> "class A { val x: Int = \"\" }"))
    assertTrue(g.typerReportedErrors)
    val bounds = "class B[T <: String]\nclass C { def f: B[Int] = null }\n"
    val o = runOn(g, r, List("b.scala" -> bounds))
    assertTrue(s"refchecks bounds check must run normally in the clean second run:\n${o.render}", o.errors.nonEmpty)
    assertFalse("flag leaked", g.typerReportedErrors)
  }

  /** After a run that stopped on typer errors, `globalPhase`/`phase` must be left where the unmodified driver loop leaves
   *  them (the phase after typer). The PR's restructured loop keeps advancing `globalPhase` to the end of the pipeline, which
   *  changes what phase later symbol lookups on a reused Global (REPL, presentation compiler, sbt) are performed at: this is
   *  what changed the `final package test` entries in the PR's edits to test/files/presentation/scope-completion*.check.
   */
  @Test def phaseAfterFailedRun_flagOff(): Unit = phaseAfterFailedRun(flag = false)
  @Test def phaseAfterFailedRun_flagOn(): Unit = phaseAfterFailedRun(flag = true)
  private def phaseAfterFailedRun(flag: Boolean): Unit = {
    val (g, r) = newGlobal(defaultArgs ++ (if (flag) List(Flag) else Nil))
    runOn(g, r, List("a.scala" -> "class A { val x: Int = \"\" }"))
    // unmodified 2.13.x driver: loop exits right after typer, `globalPhase` is typer.next
    assertEquals(s"flag=$flag globalPhase=${g.globalPhase.name} phase=${g.phase.name}", if (flag) "patmat" else "superaccessors", g.globalPhase.name) // flag: refchecks ran, loop stops after it
  }

  /** The flag must be a no-op when off. */
  @Test def flagOff_noRefchecksOnTyperErrors(): Unit = {
    val c = compare("trait T { def f: Int }\nclass C extends T { val x: Int = \"\" }\n")
    assertFalse(c.baseline.diags.exists(_.msg.contains("needs to be abstract")))
  }

  /** The presentation compiler uses its own driver, so the PR's `Global.compileUnitsInternal` change is not exercised. */
  @Test def presentationCompiler_getsRefchecksDiagnostics(): Unit = {
    val code = "trait T { def f: Int }\nclass C extends T { val x: Int = \"\" }\n"
    val ds = presentationCompiler(code, flag = true)
    assertTrue(s"PC reported no refchecks diagnostics with the flag on:\n${ds.mkString("\n")}", ds.exists(_.msg.contains("needs to be abstract")))
  }
}
