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
trait TypingTransformers extends TypeAssigners {

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
    // The owner before frame `i` was entered, restored when it exits.
    private[this] var prevOwners     = new Array[AnyRef](16)
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

    /** The context of frame `i`, made now if it was not. A typer on it stays valid while the frame is entered. */
    protected def frameContext(i: Int): analyzer.Context = {
      materialize()
      if (i + 1 < frames) parentContexts(i + 1).asInstanceOf[analyzer.Context] else _localTyper.context
    }

    currentOwner = _localTyper.context.owner
    protected var curTree: Tree = _

    @inline override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    /** `trans` with `owner` (and `tree`) entered. Final and `@inline`, so that the optimizer inlines it at each
     *  call site and eliminates the closure of `trans`; the work is in `enterOwner` and `exitOwner`.
     */
    @inline final def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val i = enterOwner(tree, owner)
      try trans finally exitOwner(i)
    }

    /** Pushes a frame for `tree` and `owner` and makes `owner` the current owner, as `Transformer.atOwner` does.
     *  Returns the frame's index for `exitOwner`. A subclass that needs more state per owner extends this pair.
     *  (Not JVM-private: `atOwner` is inlined into other classes.)
     */
    protected def enterOwner(tree: Tree, owner: Symbol): Int = {
      val i = frames
      if (i == frameTrees.length) {
        frameTrees = java.util.Arrays.copyOf(frameTrees, i * 2)
        frameOwners = java.util.Arrays.copyOf(frameOwners, i * 2)
        parentContexts = java.util.Arrays.copyOf(parentContexts, i * 2)
        prevOwners = java.util.Arrays.copyOf(prevOwners, i * 2)
      }
      frameTrees(i) = tree
      frameOwners(i) = if (owner.isModuleNotMethod) owner.moduleClass else owner
      prevOwners(i) = currentOwner
      frames = i + 1
      if (eager) materialize()
      currentOwner = owner
      i
    }

    /** Pops frame `i`, restoring the owner and, if it was made, the context from before it. */
    protected def exitOwner(i: Int): Unit = {
      currentOwner = prevOwners(i).asInstanceOf[Symbol]
      if (materialized > i) {
        _localTyper.context = parentContexts(i).asInstanceOf[analyzer.Context]
        parentContexts(i) = null
        materialized = i
      }
      frameTrees(i) = null
      frameOwners(i) = null
      prevOwners(i) = null
      frames = i
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
    def transformAtOwner(owner: Symbol, tree: Tree): Tree = atOwner(tree, owner) { transform(tree) }
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

