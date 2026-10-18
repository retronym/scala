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

import scala.collection.mutable


/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  protected def newRootLocalTyper(unit: CompilationUnit): analyzer.Typer = if (phase.erasedTypes)
    erasure.erasureAnalyzer.newTyper(erasure.erasureAnalyzer.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
  else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
    analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))

  /** A transformer with a typer whose context follows the trees it descends into.
   *
   *  Entering an owner (`atOwner`) only pushes the tree and owner onto a stack; the context for them is made
   *  (`Context.make`, as it was eagerly) when `localTyper` is next read. Most definitions in most phases are
   *  transformed without typing anything, so most of those contexts were never used.
   */
  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends AstTransformer {
    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))

    private[this] var _localTyper: analyzer.Typer = initLocalTyper
    // The frames entered by `atOwner`. Those below `materialized` have their context made, and `parentContexts(i)` is
    // the context that frame `i`'s was made from, restored when it exits.
    private[this] var frameTrees     = new Array[AnyRef](16)
    private[this] var frameOwners    = new Array[AnyRef](16)
    private[this] var parentContexts = new Array[AnyRef](16)
    private[this] var frames         = 0
    private[this] var materialized   = 0
    private[this] val eager          = settings.YeagerTypingTransformerContexts.value

    /** The typer, its context made for the innermost owner entered. */
    def localTyper: analyzer.Typer = {
      if (materialized < frames) materialize()
      _localTyper
    }
    def localTyper_=(typer: analyzer.Typer): Unit = {
      if (materialized < frames) materialize()
      _localTyper = typer
    }

    private def materialize(): Unit =
      while (materialized < frames) {
        val i = materialized
        val context = _localTyper.context
        parentContexts(i) = context
        _localTyper.context = context.make(frameTrees(i).asInstanceOf[Tree], frameOwners(i).asInstanceOf[Symbol])
        materialized = i + 1
      }

    currentOwner = _localTyper.context.owner
    protected var curTree: Tree = _

    override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    /** Transforms with `owner` entered for `tree`: `pushOwner(tree, owner)`, `trans`, `popOwner`. */
    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val prev = pushOwner(tree, owner)
      val result = trans
      popOwner(prev)
      result
    }

    override final def pushOwner(owner: Symbol): Symbol = pushOwner(curTree, owner)

    /** Enters `owner` for `tree`: pushes a frame, whose context is made when `localTyper` is next read. */
    def pushOwner(tree: Tree, owner: Symbol): Symbol = {
      val i = frames
      if (i == frameTrees.length) {
        frameTrees = java.util.Arrays.copyOf(frameTrees, i * 2)
        frameOwners = java.util.Arrays.copyOf(frameOwners, i * 2)
        parentContexts = java.util.Arrays.copyOf(parentContexts, i * 2)
      }
      frameTrees(i) = tree
      frameOwners(i) = if (owner.isModuleNotMethod) owner.moduleClass else owner
      frames = i + 1
      if (eager) materialize()
      super.pushOwner(owner)
    }

    /** Leaves the innermost frame, restoring the context it was made from if it was made. */
    override def popOwner(prev: Symbol): Unit = {
      super.popOwner(prev)
      val i = frames - 1
      if (materialized > i) {
        _localTyper.context = parentContexts(i).asInstanceOf[analyzer.Context]
        parentContexts(i) = null
        materialized = i
      }
      frameTrees(i) = null
      frameOwners(i) = null
      frames = i
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          transformChildrenAtOwner(currentOwner, tree)
        case PackageDef(_, _) =>
          transformChildrenAtOwner(tree.symbol, tree)
        case _ =>
          tree.transform(this)
      }
    }

    /** `tree.transform(this)` with `owner` entered for `tree`; by `atOwner` for a subclass that overrides it. */
    private def transformChildrenAtOwner(owner: Symbol, tree: Tree): Tree =
      if (pushPopOwners) {
        val prev = pushOwner(tree, owner)
        val result = tree.transform(this)
        popOwner(prev)
        result
      } else atOwner(tree, owner)(tree.transform(this))

    def transformAtOwner(owner: Symbol, tree: Tree): Tree =
      if (pushPopOwners) {
        val prev = pushOwner(tree, owner)
        val result = transform(tree)
        popOwner(prev)
        result
      } else atOwner(tree, owner)(transform(tree))
  }

  private object ThicketAttachment
  /** A base class for typing transformers that need to perform "thicket expansion". A thicket is the output of a
   *  transformation that is flattened into the enclosing block.
   */
  abstract class ThicketTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper) {
    private def expandThicket(t: Tree): List[Tree] = t match {
      case Block(stats, expr) if t.attachments.containsElement(ThicketAttachment) =>
        stats :+ expr
      case _ => t :: Nil
    }

    def apply(tree: Tree): List[Tree] = expandThicket(transform(tree))

    protected def Thicket(stats: List[Tree], expr: Tree): Tree = {
      Block(stats, expr).updateAttachment(ThicketAttachment)
    }
    protected def Thicket(block: Block): Tree = {
      block.updateAttachment(ThicketAttachment)
    }

    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) =>
        val transformedStats = transformTrees(stats)
        val transformedExpr = transform(expr)
        if ((stats eq transformedStats) && (expr eq transformedExpr)) tree
        else {
          val expanded = new mutable.ListBuffer[Tree]
          def expandStats(): Unit = transformedStats.foreach {
            case EmptyTree =>
            case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
              stats.foreach { s => if (s != EmptyTree) expanded += s }
              if (expr != EmptyTree) expanded += expr
            case t =>
              expanded += t
          }
          def expandExpr(): Tree = transformedExpr match {
            case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
              stats.foreach { s => if (s != EmptyTree) expanded += s }
              expr
            case t =>
              t
          }
          expandStats()
          val expr1 = expandExpr()
          treeCopy.Block(tree, expanded.toList, expr1)
        }
      case _ =>
        super.transform(tree)
    }
  }
}

