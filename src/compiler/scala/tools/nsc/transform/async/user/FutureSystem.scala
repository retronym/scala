/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async.user

import scala.language.higherKinds
import scala.reflect.internal.{AnnotationInfos, NoPhase, SymbolTable}
import scala.tools.nsc.Global

/**
 * An abstraction over a future system.
 *
 * Used by the macro implementations in [[scala.tools.nsc.transform.async.AsyncTransform]] to
 * customize the code generation.
 *
 * The API mirrors that of `scala.concurrent.Future`, see the instance
 * [[ScalaConcurrentFutureSystem]] for an example of how
 * to implement this.
 */
trait FutureSystem {
  /** A container to receive the final value of the computation */
  type Prom[A]
  /** A (potentially in-progress) computation */
  type Fut[A]
  /** An execution context, required to create or register an on completion callback on a Future. */
  type ExecContext
  /** Any data type isomorphic to scala.util.Try. */
  type Tryy[T]

  lazy val futureSystemName = {
    val n = getClass.getName
    if(n.endsWith("$")) n.substring(0, n.length-1) else n
  }

  abstract class Ops[Universe <: Global](val u: Universe) {
    import u._

    final def isPastErasure = {
      val global = u.asInstanceOf[Global]
      val erasurePhase = global.currentRun.erasurePhase
      erasurePhase != NoPhase && global.isPast(erasurePhase)
    }

    protected[this] def isAsyncOrAwait(fun: Tree, anno: Symbol): Boolean = {
      val sym = fun.symbol
      sym != null && {
        val so = fun.symbol.getAnnotation(anno)
        so.isDefined && so.get.stringArg(0).getOrElse("") == futureSystemName
      }
    }

    def isAwait(fun: Tree) = isAsyncOrAwait(fun, currentRun.runDefinitions.Async_awaitMethod)

    def literalUnitExpr = if (isPastErasure) gen.mkAttributedRef(definitions.BoxedUnit_UNIT) else Literal(Constant(()))

    def phasedAppliedType(tycon: Type, tp: Type) = if (isPastErasure) tycon else appliedType(tycon, tp)

    def tryType(tp: Type): Type
    def tryTypeToResult(tp: Type): Type
    def stateMachineClassParents: List[Type] = Nil

    /** Construct a future to asynchronously compute the given expression -- tree shape should take isPastErasure into account */
    def future(a: Tree, execContext: Tree): Tree
    def futureUnit(execContext: Tree): Tree

    /** Return a Prom[A] type for A */
    def promType(tp: Type): Type

    /** Create a Prom[A] instance for A */
    def createProm[A](resultType: Type): Expr[Prom[A]]

    def promiseToFuture[A](prom: Expr[Prom[A]]): Expr[Fut[A]]

    /** Create a default execution context value (only used if the "async" method does not take an execution context parameter) */
    def defaultExecContext: Tree = ???

    type Expr[T] = Tree

    /** Register an call back to run on completion of the given future -- only called when isPastErasure */
    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[Tryy[A] => B],
                         execContext: Expr[ExecContext], aTp: Type): Expr[Unit]

    def continueCompletedFutureOnSameThread = false

    /** Return `null` if this future is not yet completed, or `Tryy[A]` with the completed result
      * otherwise
      *
      * Only called when isPastErasure
      */
    def getCompleted[A](future: Expr[Fut[A]]): Expr[Tryy[A]] =
      throw new UnsupportedOperationException("getCompleted not supported by this FutureSystem")

    /** Complete a promise with a value -- only called when isPastErasure */
    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit]
    def completeWithSuccess[A](prom: Expr[Prom[A]], value: Expr[A], aTp: Type): Expr[Unit] = completeProm(prom, tryySuccess(value, aTp))

    def tryyIsFailure[A](tryy: Expr[Tryy[A]]): Expr[Boolean]

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A]
    def tryySuccess[A](a: Expr[A], aTp: Type): Expr[Tryy[A]]
    def tryyFailure[A](a: Expr[Throwable]): Expr[Tryy[A]]

    /** A hook for custom macros to transform the tree post-ANF transform */
    def postAnfTransform(tree: Block): Block = tree

    /** A hook for custom macros to selectively generate and process a Graphviz visualization of the transformed state machine */
    def dot(enclosingOwner: Symbol, macroApplication: Tree): Option[(String => Unit)] = None

  }

  def mkOps(u: Global): Ops[u.type]

  @deprecated("No longer honoured by the macro, all generated names now contain $async to avoid accidental clashes with lambda lifted names", "0.9.7")
  def freshenAllNames: Boolean = false
  def emitTryCatch: Boolean = true
  @deprecated("No longer honoured by the macro, all generated names now contain $async to avoid accidental clashes with lambda lifted names", "0.9.7")
  def resultFieldName: String = "result"
}

// TODO AM: test the erased version by running the remainder of the test suite post-posterasure (i.e., not LateExpansion, which tests AsyncId)
object ScalaConcurrentFutureSystem extends FutureSystem {
  import scala.concurrent._

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext
  type Tryy[A] = scala.util.Try[A]

  def mkOps(u: Global): Ops[u.type] = new ScalaConcurrentOps[u.type](u)
  class ScalaConcurrentOps[Universe <: Global](u0: Universe) extends Ops[Universe](u0) {
    import u._

    private val global = u.asInstanceOf[Global]
    lazy val Future_class: Symbol = rootMirror.requiredClass[scala.concurrent.Future[_]]
    lazy val Option_class: Symbol = rootMirror.requiredClass[scala.Option[_]]
    lazy val Promise_class: Symbol = rootMirror.requiredClass[scala.concurrent.Promise[_]]
    lazy val Try_class: Symbol = rootMirror.requiredClass[scala.util.Try[_]]
    lazy val Success_class: Symbol = rootMirror.requiredClass[scala.util.Success[_]]
    lazy val Failure_class: Symbol = rootMirror.requiredClass[scala.util.Failure[_]]
    lazy val Future_onComplete: Symbol = Future_class.info.member(TermName("onComplete"))
    lazy val Future_value: Symbol = Future_class.info.member(TermName("value"))
    lazy val Future_isCompleted: Symbol = Future_class.info.member(TermName("isCompleted"))
    lazy val Future_unit: Symbol = Future_class.companionModule.info.member(TermName("unit"))
    lazy val Option_get: Symbol = Option_class.info.member(TermName("get"))
    lazy val Promise_complete: Symbol = Promise_class.info.member(TermName("complete"))
    lazy val Try_isFailure: Symbol = Try_class.info.member(TermName("isFailure"))
    lazy val Try_get: Symbol = Try_class.info.member(TermName("get"))

    def tryType(tp: Type): Type = appliedType(Try_class, tp)
    def tryTypeToResult(tp: Type): Type = tp.baseType(Try_class).typeArgs.headOption.getOrElse(NoType)

    def future(a: Tree, execContext: Tree): Tree =
      if (isPastErasure) Apply(Select(gen.mkAttributedStableRef(Future_class.companionModule), nme.apply), List(a, execContext))
      else Apply(Apply(Select(gen.mkAttributedStableRef(Future_class.companionModule), nme.apply), List(a)), List(execContext))

    def futureUnit(execContext: Tree): Tree =
      mkAttributedSelectApplyIfNeeded(gen.mkAttributedStableRef(Future_class.companionModule), Future_unit)

    def promType(tp: Type): Type = appliedType(Promise_class, tp)

    def createProm[A](resultType: Type): Expr[Prom[A]] =
      Apply(TypeApply(gen.mkAttributedStableRef(Promise_class.companionModule), TypeTree(resultType) :: Nil), Nil)

    def promiseToFuture[A](prom: Expr[Prom[A]]): Expr[Fut[A]] = Select(prom, nme.future)

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => B],
                         execContext: Expr[ExecContext], aTp: Type): Expr[Unit] = {
      val sel = Select(future, Future_onComplete)
      if (isPastErasure)
        Apply(sel, fun :: execContext :: Nil)
      else
        Apply(Apply(TypeApply(sel, TypeTree(definitions.UnitTpe) :: Nil), fun :: Nil), execContext :: Nil)
    }

    override def continueCompletedFutureOnSameThread: Boolean = true
    
    def mkAttributedSelectApplyIfNeeded(qual: Tree, sym: Symbol) = {
      val sel = gen.mkAttributedSelect(qual, sym)
      if (isPastErasure) Apply(sel, Nil) else sel
    }

    override def getCompleted[A](future: Expr[Fut[A]]): Expr[Tryy[A]] = {
      val futVal = mkAttributedSelectApplyIfNeeded(future, Future_value)
      val futValGet = mkAttributedSelectApplyIfNeeded(futVal, Option_get)
      val isCompleted = mkAttributedSelectApplyIfNeeded(future, Future_isCompleted)
      If(isCompleted, futValGet, Literal(Constant(null)))
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = {
      gen.mkMethodCall(prom, Promise_complete, Nil, value :: Nil)
    }

    def tryyIsFailure[A](tryy: Expr[scala.util.Try[A]]): Expr[Boolean] = {
      mkAttributedSelectApplyIfNeeded(tryy, Try_isFailure)
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      mkAttributedSelectApplyIfNeeded(tryy, Try_get)
    }

    def tryySuccess[A](a: Expr[A], aTp: Type): Expr[Tryy[A]] = {
      assert(isPastErasure)
      New(Success_class, a)
    }

    def tryyFailure[A](a: Expr[Throwable]): Expr[Tryy[A]] = {
      assert(isPastErasure)
      New(Failure_class, a)
    }
  }
}
