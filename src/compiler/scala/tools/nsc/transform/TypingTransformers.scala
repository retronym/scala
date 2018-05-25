/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.collection.mutable

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer =
      if (phase.erasedTypes)
        erasure.newTyper(erasure.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
      else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
        analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
    protected var curTree: Tree = _

    override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedContext = localTyper.context
      localTyper.context = localTyper.context.make(tree, if (owner.isModuleNotMethod) owner.moduleClass else owner)
      val result = super.atOwner(owner)(trans)
      localTyper.context = savedContext
      result
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          atOwner(currentOwner) { tree.transform(this) }
        case PackageDef(_, _) =>
          atOwner(tree.symbol) { tree.transform(this) }
        case _ =>
          tree.transform(this)
      }
    }
  }

  abstract class TypingTransformerFast(unit: CompilationUnit) extends TransformerFast {
    val localTyper =
      if (phase.erasedTypes)
        erasure.newTyper(erasure.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
      else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
        analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
    val contexts = mutable.ArrayBuffer[analyzer.Context](localTyper.context)
    protected var curTree: Tree = _

    override def pushOwner(owner: Symbol): Unit = {
      pushOwner(curTree, owner)
    }
    def pushOwner(tree: Tree, owner: Symbol): Unit = {
      super.pushOwner(owner)
      val context = contexts.last.make(tree, if (owner.isModuleNotMethod) owner.moduleClass else owner)
      localTyper.context = context
      contexts += context
    }
    override def popOwner(): Unit = {
      contexts.remove(contexts.length - 1)
      localTyper.context = contexts.last
      super.popOwner()
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          pushOwner(currentOwner)
          try tree.transformFast(this)
          finally popOwner()
        case PackageDef(_, _) =>
          pushOwner(tree.symbol)
          try tree.transformFast(this)
          finally popOwner()
        case _ =>
          tree.transformFast(this)
      }
    }
  }
}

