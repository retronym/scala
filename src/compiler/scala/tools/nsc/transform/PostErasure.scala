/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

/** This phase maps ErasedValueTypes to the underlying unboxed representation and
 *  performs peephole optimizations.
 */
trait PostErasure extends InfoTransform with TypingTransformers with scala.reflect.internal.transform.PostErasure {
  val global: Global

  import global._
  import treeInfo._

  val phaseName: String = "posterasure"

  def newTransformer(unit: CompilationUnit): AstTransformer = new PostErasureTransformer(unit)
  override def changesBaseClasses = false

  class PostErasureTransformer(unit: CompilationUnit) extends ContextLiteTransformer with ContextLiteTypeAssigner {
    override def transform(tree: Tree) = {
      def finish(res: Tree) = logResult(s"Posterasure reduction\n  Old: $tree\n  New")(res)

      /* We use the name of the operation being performed and not the symbol
       * itself because the symbol hails from the boxed class, and this transformation
       * exists to operate directly on the values. So we are for instance looking
       * up == on an lhs of type Int, whereas the symbol which has been passed in
       * is from java.lang.Integer.
       */
      def binop(lhs: Tree, op: Symbol, rhs: Tree) =
        finish(typed(Apply(Select(lhs, primitiveOp(lhs.tpe, op.name, rhs.tpe)) setPos tree.pos, rhs :: Nil) setPos tree.pos, WildcardType))

      /* The member `name` of `lhs` that takes an `rhs`, as `Typer` resolves the overload: the one
       * whose parameter type is `rhs`, else the only one that accepts it.
       */
      def primitiveOp(lhs: Type, name: Name, rhs: Type): Symbol = {
        val alts = lhs.member(name).alternatives
        def unique(p: Type => Boolean) = alts.filter(_.paramss match {
          case List(List(param)) => p(param.tpe)
          case _ => false
        }) match {
          case List(op) => op
          case _ => NoSymbol
        }
        unique(rhs =:= _) orElse unique(rhs <:< _) orElse abort(s"$phase: no unique $name in $lhs taking $rhs: $alts")
      }

      super.transform(tree) setType elimErasedValueType(tree.tpe) match {
        case AsInstanceOf(v, tpe) if v.tpe <:< tpe => finish(v)          // x.asInstanceOf[X]       ==> x
        case ValueClass.BoxAndUnbox(v)             => finish(v)          // (new B(v)).unbox        ==> v
        case ValueClass.BoxAndCompare(v1, op, v2)  => binop(v1, op, v2)  // new B(v1) == new B(v2)  ==> v1 == v2
        case transformed                           => transformed
      }
    }
  }
}
