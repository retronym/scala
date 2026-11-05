package scala.reflect.internal

import org.junit.Assert.fail
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

/** `lubList` assumes that base type sequences are sorted by `Symbol#isLess`, and loses from the lub
 *  a type that is out of order. An abstract type's position in that order must hold whichever class
 *  its bound turns out to be seen from a prefix, however long that class's base type sequence is
 *  compared to the bound as declared, and whichever symbol was created first.
 *
 *  So this checks a grid of the shapes that can get that wrong, rather than one example.
 */
object BaseTypeSeqOrderTest {
  /** @param apiBound     the bound of the abstract `Name` where it is declared
   *  @param implParents  how many more traits the class `Name` extends, which sets the length of its
   *                      base type sequence compared to `apiBound`, and so can make the ranks tie
   *  @param memberBound  the bound of the abstract type member whose bound is `Name`, one way or another
   *  @param apiFirst     which of `Api` and `Impl` is declared first, which decides symbol ids in a tie
   */
  final case class Shape(apiBound: String, implParents: Int, memberBound: String, apiFirst: Boolean) {
    private def api =
      s"""trait Api {
         |  trait NameApi; trait E; trait Marker
         |  type Name >: Null <: $apiBound
         |  trait Sym {
         |    type U >: Null <: Name
         |    type NameType >: Null <: $memberBound
         |    def name: NameType
         |  }
         |}
         |""".stripMargin

    private def impl = {
      val traits = List.tabulate(implParents)(i => s"T$i")
      val decls = traits.map("trait " + _).mkString("; ")
      val parents = ("NameApi" :: "E" :: traits).mkString(" with ")
      s"""trait Impl extends Api {
         |  $decls
         |  abstract class Name extends $parents
         |  class TermName extends Name
         |  val s: Sym
         |  def classBound: Name
         |  def termName: TermName
         |  def memberName: s.NameType
         |  val s2: Sym
         |  def otherMemberName: s2.NameType
         |  def boundType: Name
         |  def inferred(b: Boolean) = if (b) termName else s.name
         |}
         |""".stripMargin
    }

    def source: String = if (apiFirst) api + impl else impl + api
    def describe: String = s"apiBound=[$apiBound] implParents=$implParents memberBound=[$memberBound] apiFirst=$apiFirst"
  }

  val shapes: List[Shape] =
    for {
      apiBound    <- List("AnyRef", "AnyRef with NameApi", "NameApi", "NameApi with E")
      implParents <- List(0, 1, 2, 3, 4)
      memberBound <- List("Name", "Name with Marker", "U", "Name { def x: Int }")
      apiFirst    <- List(true, false)
    } yield Shape(apiBound, implParents, memberBound, apiFirst)
}

class BaseTypeSeqOrderTest extends BytecodeTesting {
  import BaseTypeSeqOrderTest._

  override def compilerArgs = "-Ystop-after:typer"

  import compiler.global
  import global._

  /** An abstract type is followed by its bound, and seen from a prefix a refinement there can rank ahead
   *  of it. The frontier in `lubList` only loses `b` from `a, b` if another operand also has `b`, and it
   *  can't, as this refinement is a symbol of its own.
   */
  private def isHarmless(a: Type, b: Type) = a.typeSymbol.isAbstractType && b.typeSymbol.isRefinementClass

  private def isSorted(tp: Type): List[String] = {
    val elems = tp.baseTypeSeq.toList
    elems.zip(elems.tail).collect {
      case (a, b) if !(a.typeSymbol isLess b.typeSymbol) && !isHarmless(a, b) => s"base types of $tp are out of order: $a before $b"
    }
  }

  private def isStrictTotalOrder(syms: List[Symbol]): List[String] = {
    val irreflexive = syms.collect { case a if a isLess a => s"$a isLess itself" }
    val total = for {
      a <- syms; b <- syms if a ne b
      if (a isLess b) == (b isLess a)
    } yield s"exactly one of $a isLess $b and the converse must hold"
    val transitive = for {
      a <- syms; b <- syms; c <- syms
      if (a isLess b) && (b isLess c) && !(a isLess c)
    } yield s"$a isLess $b isLess $c, but not $a isLess $c"
    irreflexive ::: total ::: transitive
  }

  /** The problems with this shape, and the type it infers. */
  private def check(shape: Shape): (List[String], String) = {
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(shape.source, "unitTestSource.scala")))
    compiler.checkReport()
    exitingTyper {
      val impl = rootMirror.EmptyPackageClass.info.member(TypeName("Impl"))
      def resultOf(name: String) = impl.thisType.memberInfo(impl.info.member(TermName(name))).finalResultType
      val classBound = resultOf("classBound")
      val termName = resultOf("termName")
      val memberName = resultOf("memberName")
      val inferred = resultOf("inferred")

      val operands = List(termName, memberName)
      val syms = (classBound :: operands).flatMap(_.baseTypeSeq.toList.map(_.typeSymbol)).distinct

      def keepsBound(what: String, tp: Type) =
        if (tp <:< classBound) Nil else List(s"$what is $tp, which does not conform to $classBound")

      val problems =
        operands.flatMap(isSorted) :::
        isStrictTotalOrder(syms) :::
        keepsBound("the lub", lub(operands)) :::
        keepsBound("the lub with operands swapped", lub(operands.reverse)) :::
        keepsBound("the inferred type", inferred) :::
        keepsBound("the lub of two prefixes", lub(List(memberName, resultOf("otherMemberName")))) :::
        keepsBound("the lub of three", lub(List(termName, memberName, resultOf("otherMemberName")))) :::
        keepsBound("the lub with the bound", lub(List(memberName, resultOf("boundType"))))
      (problems, inferred.toString)
    }
  }

  @Test def baseTypeSeqsAreSortedAndLubsKeepTheClassInABound(): Unit = {
    val results = shapes.map(shape => shape -> check(shape))

    val problems = results.flatMap { case (shape, (problems, _)) => problems.map(shape.describe + ": " + _) }

    val orderDependent = results
      .groupBy { case (shape, _) => shape.copy(apiFirst = true) }
      .toList
      .collect { case (shape, both) if both.map(_._2._2).distinct.lengthIs > 1 =>
        s"${shape.copy(apiFirst = true).describe}: inferred types depend on declaration order: ${both.map(_._2._2).distinct.mkString(" vs ")}"
      }

    val all = problems ::: orderDependent
    if (all.nonEmpty)
      fail(s"${all.size} problems, the first of which are:\n${all.take(10).mkString("\n")}")
  }
}
