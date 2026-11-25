package scala.tools.nsc
package typechecker

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.BytecodeTesting

object ApplicableCloneFixtures {
  trait HasT { type T }

  // No type param's info mentions another one: inferred through the method's own type params.
  def independent[A, B](a: A, b: B): Set[B] = ???
  def fBounded[A <: Comparable[A]](a: A): Set[A] = ???
  def fBoundedAndIndependent[A <: Comparable[A], B](a: A, b: B): Set[B] = ???

  // Inference may rewrite a type param's bounds: inferred through clones.
  def upper[A, B <: List[A]](a: A): Set[B] = ???
  def lower[A, B >: List[A]](a: A): Set[B] = ???
  def forwardReference[B <: List[A], A](a: A): Set[B] = ???
  def higherKinded[A, F[X <: A]](a: A): Set[F[A]] = ???
  def refinement[A, B <: HasT { type T = A }](a: A): Set[B] = ???
  def existential[A, B <: List[_ <: A]](a: A): Set[B] = ???
}

/** Where `Infer.isApplicable` clones a polymorphic alternative's type params, and that it never
 *  changes their infos. See `Inferencer.needsFreshTypeParams`. */
@RunWith(classOf[JUnit4])
class ApplicableCloneTest extends BytecodeTesting {
  import compiler.global._
  import analyzer._
  import definitions._

  private def fixture(name: String): Type =
    typeOf[ApplicableCloneFixtures.type].member(TermName(name)).info

  private def nextSymbolId(): Int = NoSymbol.newTypeParameter(TypeName("probe")).id

  private def infoString(tparam: Symbol): String = {
    tparam.info.typeParams.foreach(_.info) // complete a higher-kinded type param's own params, or they print as <?>
    tparam.info.toString
  }

  /** Runs `isApplicableSafe` twice and returns how many symbols the second run created
   *  (the first one also completes the symbols it reaches). */
  private def check(name: String, argtpes: List[Type], undetparams: Type => List[Symbol] = _ => Nil): Int = {
    val tpe     = fixture(name)
    val tparams = tpe.typeParams
    val infos   = tparams.map(infoString)
    def run()   = typer.infer.isApplicableSafe(undetparams(tpe), tpe, argtpes, WildcardType)
    run()
    val before  = nextSymbolId()
    run()
    val created = nextSymbolId() - before - 1
    assertEquals(s"$name: type param infos changed", infos, tparams.map(infoString))
    created
  }

  private def assertNoClone(name: String, argtpes: List[Type]): Unit =
    assertEquals(s"$name cloned its type params", 0, check(name, argtpes))

  private def assertClone(name: String, argtpes: List[Type], undetparams: Type => List[Symbol] = _ => Nil): Unit = {
    val arity = fixture(name).typeParams.length
    val created = check(name, argtpes, undetparams)
    assertTrue(s"$name did not clone its type params (created $created symbols)", created >= arity)
  }

  @Test def independentTypeParams(): Unit = enteringPhase(new Run().typerPhase) {
    assertNoClone("independent", List(IntTpe, StringTpe))
    assertNoClone("independent", List(NothingTpe, NothingTpe))
  }

  @Test def selfReferentialBound(): Unit = enteringPhase(new Run().typerPhase) {
    assertNoClone("fBounded", List(StringTpe))
    assertNoClone("fBoundedAndIndependent", List(StringTpe, IntTpe))
  }

  @Test def boundMentionsSibling(): Unit = enteringPhase(new Run().typerPhase) {
    for {
      name   <- List("upper", "lower", "forwardReference", "higherKinded", "refinement", "existential")
      argtpe <- List(IntTpe, NothingTpe)
    } assertClone(name, List(argtpe))
  }

  @Test def typeParamsAlreadyUndetermined(): Unit = enteringPhase(new Run().typerPhase) {
    assertClone("independent", List(IntTpe, StringTpe), undetparams = _.typeParams)
  }
}
