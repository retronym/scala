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
  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends OwnerStackTransformer {
    /** A transformer whose root typer for `unit` is made when it is first needed. */
    def this(unit: CompilationUnit) = {
      this(null: analyzer.Typer)
      rootUnit = unit
      if (unit.hasXml) localTyper // making the root context reports a missing scala.xml, as it did eagerly
    }

    private[this] var _localTyper: analyzer.Typer = initLocalTyper
    // Without an initial typer, the unit to make the root typer for.
    protected[this] var rootUnit: CompilationUnit = _
    // The frames below `materialized` have their context made, and `parentContexts(i)` is the context that frame
    // `i`'s was made from, restored when it exits.
    private[this] var parentContexts = new Array[AnyRef](16)
    private[this] var materialized   = 0

    /** The typer, its context made for the innermost owner entered. */
    def localTyper: analyzer.Typer = {
      if (materialized < frames || (_localTyper eq null)) materialize()
      _localTyper
    }
    def localTyper_=(typer: analyzer.Typer): Unit = {
      if (materialized < frames) materialize()
      _localTyper = typer
    }

    /** The owner of `localTyper`'s context, without making it. */
    protected def localTyperOwner: Symbol =
      if (materialized < frames) frameOwners(frames - 1).asInstanceOf[Symbol]
      else if (_localTyper eq null) rootMirror.RootClass
      else _localTyper.context.owner

    /** The typer to start from, made when first needed if the transformer was given none. */
    protected def initialTyper(): analyzer.Typer = newRootLocalTyper(rootUnit)

    private def materialize(): Unit = {
      if (_localTyper eq null) _localTyper = initialTyper()
      while (materialized < frames) {
        val i = materialized
        val context = _localTyper.context
        parentContexts(i) = context
        _localTyper.context = context.make(frameTrees(i).asInstanceOf[Tree], frameOwners(i).asInstanceOf[Symbol])
        materialized = i + 1
      }
    }

    /** The context of frame `i`, made now if it was not. A typer on it stays valid while the frame is entered. */
    protected def frameContext(i: Int): analyzer.Context = {
      materialize()
      if (i + 1 < frames) parentContexts(i + 1).asInstanceOf[analyzer.Context] else _localTyper.context
    }

    currentOwner = if (_localTyper eq null) rootMirror.RootClass else _localTyper.context.owner

    override protected def enterOwner(tree: Tree, owner: Symbol): Int = {
      val i = super.enterOwner(tree, owner)
      if (i == parentContexts.length) parentContexts = java.util.Arrays.copyOf(parentContexts, i * 2)
      i
    }

    /** Pops frame `i`, restoring the owner and, if it was made, the context from before it. */
    override protected def exitOwner(i: Int): Unit = {
      super.exitOwner(i)
      if (materialized > i) {
        _localTyper.context = parentContexts(i).asInstanceOf[analyzer.Context]
        parentContexts(i) = null
        materialized = i
      }
    }
  }

  /** A transformer that keeps a stack of the trees and owners it has entered, with no typing context.
   *  `TypingTransformer` makes contexts for its frames.
   */
  abstract class OwnerStackTransformer extends AstTransformer {
    // The frames entered by `atOwner`, and the owner before frame `i` was entered, restored when it exits.
    protected[this] var frameTrees  = new Array[AnyRef](16)
    protected[this] var frameOwners = new Array[AnyRef](16)
    private[this] var prevOwners    = new Array[AnyRef](16)
    protected[this] var frames      = 0

    protected var curTree: Tree = _

    override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    /** `trans` with `owner` (and `tree`) entered; the work is in `enterOwner` and `exitOwner`. */
    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val i = enterOwner(tree, owner)
      try trans finally exitOwner(i)
    }

    /** Pushes a frame for `tree` and `owner` and makes `owner` the current owner, as `Transformer.atOwner` does.
     *  Returns the frame's index for `exitOwner`. A subclass that needs more state per owner extends this pair.
     */
    protected def enterOwner(tree: Tree, owner: Symbol): Int = {
      val i = frames
      if (i == frameTrees.length) {
        frameTrees = java.util.Arrays.copyOf(frameTrees, i * 2)
        frameOwners = java.util.Arrays.copyOf(frameOwners, i * 2)
        prevOwners = java.util.Arrays.copyOf(prevOwners, i * 2)
      }
      frameTrees(i) = tree
      frameOwners(i) = if (owner.isModuleNotMethod) owner.moduleClass else owner
      prevOwners(i) = currentOwner
      frames = i + 1
      currentOwner = owner
      i
    }

    /** Pops frame `i`, restoring the owner from before it. */
    protected def exitOwner(i: Int): Unit = {
      currentOwner = prevOwners(i).asInstanceOf[Symbol]
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

