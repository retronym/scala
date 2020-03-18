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

package scala.tools.nsc.transform.async

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers

// NOTE: this part runs during typer to wrap argument to the `async` macro
// in a class. This will later serve as storage for state of the state
// machine, and before that will force the compiler to use outer pointers
// for references from the async block to members of its owning or other
// enclosing classes.
abstract class AsyncEarlyExpansion extends TypingTransformers {
  import global._

  private lazy val TryClass = rootMirror.requiredClass[scala.util.Try[_]]
  private lazy val FailureClass = rootMirror.requiredClass[scala.util.Failure[_]]
  private lazy val SuccessClass = rootMirror.requiredClass[scala.util.Success[_]]
  private lazy val FutureClass = rootMirror.requiredClass[scala.concurrent.Future[_]]
  private lazy val PromiseClass = rootMirror.requiredClass[scala.concurrent.Promise[_]]
  private lazy val Future_unit: Symbol = FutureClass.companionModule.info.member(TermName("unit"))
  private lazy val Future_onComplete: Symbol = FutureClass.info.member(TermName("onComplete"))
  private lazy val Future_value: Symbol = FutureClass.info.member(TermName("value"))
  private lazy val Promise_complete: Symbol = PromiseClass.info.member(TermName("complete"))
  private lazy val NonFatalClass: Symbol = rootMirror.requiredClass[scala.util.control.NonFatal.type]
  private lazy val Option_isDefined: Symbol = definitions.OptionClass.info.member(TermName("isDefined"))
  private lazy val Option_get: Symbol = definitions.OptionClass.info.member(TermName("get"))


  /** Perform async macro expansion during typers to a block that creates the state machine class,
    * along with supporting definitions, but without the ANF/Async expansion.
    *
    * The full expansion of the actual state machine logic (anf & async) is performed by asyncTransform after erasure.
    * Until then, the state machine's apply method just has the original async macro invocation.
    *
    * The goal is to balance compiler performance by delaying tree explosion (due to anf and state machine mechanics) and
    * complexity arising from deftree synthesis in later phases, which would require
    * retro-actively running erasure (bridges/types) and explicitouter (add outer arg to state machine ctor)
    * on the synthetic def trees.
    *
    * Synthesizes:
      {
        val execContext0$async: scala.concurrent.ExecutionContext = `execContext`;
        class stateMachine$async extends extends scala.runtime.AbstractFunction1[scala.util.Try[Int],Unit] {
          def <init>(): stateMachine$async = {
            stateMachine$async.super.<init>();
            ()
          };
          private[this] var state$async: Int = 0;
          <accessor> private def state$async: Int = stateMachine$async.this.state$async;
          <accessor> private def state$async_=(x$1: Int): Unit = stateMachine$async.this.state$async = x$1;
          private[this] val result$async: scala.concurrent.Promise[`resultType`] = Promise.apply[`resultType`]();
          <stable> <accessor> def result$async: scala.concurrent.Promise[`resultType`] = stateMachine$async.this.result$async;
          private[this] val execContext$async: scala.concurrent.ExecutionContext = execContext0$async;
          <stable> <accessor> def execContext$async: scala.concurrent.ExecutionContext = stateMachine$async.this.execContext$async;
          def apply(tr$async: scala.util.Try[`resultType`]): Unit = {
            `asyncBody`
            ()
          }
        };
        val stateMachine$async: stateMachine$async = new stateMachine$async();
        scala.concurrent.Future.unit.onComplete[Unit](stateMachine$async.asInstanceOf[scala.util.Try[Unit] => Unit])(stateMachine$async.execContext$async);
        stateMachine$async.result$async.future
      }
    */
  def apply(callsiteTyper: analyzer.Typer, asyncBody: Tree, execContext: Tree, resultType: Type) = {
    val tryResult = appliedType(TryClass, definitions.AnyRefTpe :: Nil)

    val execContextTempVal =
      ValDef(NoMods, nme.execContextTemp, TypeTree(execContext.tpe), execContext)

    val stateMachine: ClassDef = {
      val parents =
        List(TypeTree(definitions.abstractFunctionType(tryResult :: Nil, definitions.UnitTpe)))

      val stateVar =
        ValDef(Modifiers(Flags.MUTABLE | Flags.PRIVATE), nme.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))

      val createProm: Tree =
        Apply(TypeApply(gen.mkAttributedStableRef(PromiseClass.companionModule), TypeTree(definitions.AnyRefTpe) :: Nil), Nil)

      val resultVal =
        ValDef(NoMods, nme.result, TypeTree(appliedType(PromiseClass, definitions.AnyRefTpe)), createProm)

      val execContextVal =
        ValDef(NoMods, nme.execContext, TypeTree(execContext.tpe), Ident(nme.execContextTemp))

      val applyFSM: DefDef = {
        val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), nme.tr, TypeTree(tryResult), EmptyTree)))
        DefDef(NoMods, nme.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), Block(
          asyncBody.updateAttachment(SuppressPureExpressionWarning), Literal(Constant(())))
        ).updateAttachment(ChangeOwnerAttachment(callsiteTyper.context.owner))
      }

      val onComplete: DefDef = {
        val futureName = TermName("future")
        val vparamss = List(List(ValDef(Modifiers(Flags.PARAM), futureName, TypeTree(appliedType(FutureClass, definitions.AnyRefTpe)), EmptyTree)))
        val rhs = Apply(gen.mkMethodCall(Ident(futureName), Future_onComplete, List(definitions.UnitTpe), List(This(tpnme.EMPTY))), List(Ident(nme.execContextTemp)))
        DefDef(Modifiers(Flags.PRIVATE), TermName("onComplete"), Nil, vparamss, TypeTree(definitions.UnitTpe), rhs)
      }

      val tryGet: DefDef = {
        val trName = TermName("tr")
        val vparamss = List(List(ValDef(Modifiers(Flags.PARAM), trName, TypeTree(appliedType(TryClass, definitions.AnyRefTpe)), EmptyTree)))
        val rhs = If(Select(Ident(trName), TermName("isFailure")),
          Block(
            gen.mkMethodCall(Select(This(tpnme.EMPTY), resultVal.name), Promise_complete, List(), List(gen.mkCast(Ident(trName), appliedType(TryClass, definitions.AnyRefTpe)))) :: Nil,
            This(tpnme.EMPTY) // sentinel value to indicate the dispatch loop should exit.
          ),
          Select(Ident(trName), TermName("get"))
        )
        DefDef(Modifiers(Flags.PRIVATE), TermName("tryGet"), Nil, vparamss, TypeTree(definitions.AnyRefTpe), rhs)
      }

      val completeFailure: DefDef = {
        val trName = TermName("t")
        val vparamss = List(List(ValDef(Modifiers(Flags.PARAM), trName, TypeTree(definitions.ThrowableTpe), EmptyTree)))
        // scala-async accidentally started catching NonFatal exceptions in:
        //  https://github.com/scala/scala-async/commit/e3ff0382ae4e015fc69da8335450718951714982#diff-136ab0b6ecaee5d240cd109e2b17ccb2R411
        // TODO: fix this regression by flipping this boolean? Need to add test coverage!
        val useNonFatal = false
        val complete = gen.mkMethodCall(Select(This(tpnme.EMPTY), resultVal.name), Promise_complete, Nil, List(New(appliedType(FailureClass, definitions.AnyRefTpe), Ident(trName))))
        val rhs =
          if (useNonFatal) If(Apply(Ident(NonFatalClass.sourceModule), List(Ident(nme.t))), complete, Throw(Ident(nme.t)))
          else complete
        DefDef(Modifiers(Flags.PRIVATE), TermName("completeFailure"), Nil, vparamss, TypeTree(), rhs)
      }

      val completeSuccess: DefDef = {
        val valueName = TermName("value")
        val vparamss = List(List(ValDef(Modifiers(Flags.PARAM), valueName, TypeTree(definitions.AnyRefTpe), EmptyTree)))
        val complete = gen.mkMethodCall(Select(This(tpnme.EMPTY), resultVal.name), Promise_complete, Nil, List(New(appliedType(SuccessClass, definitions.AnyRefTpe), Ident(valueName))))
        DefDef(Modifiers(Flags.PRIVATE), TermName("completeSuccess"), Nil, vparamss, TypeTree(), complete)
      }

      val getCompleted: DefDef = {
        val futureName = TermName("future")
        val vparamss = List(List(ValDef(Modifiers(Flags.PARAM), futureName, TypeTree(appliedType(FutureClass, definitions.AnyRefTpe)), EmptyTree)))
        val opt = ValDef(NoMods, TermName("opt"), TypeTree(), Select(Ident(futureName), Future_value))
        val rhs = Block(
          opt :: Nil,
          If(Select(Ident(opt.name), Option_isDefined), Select(Ident(opt.name), Option_get), Literal(Constant(null)))
        )
        DefDef(Modifiers(Flags.PRIVATE), TermName("getCompleted"), Nil, vparamss, TypeTree(), rhs)
      }

      async.markForAsyncTransform(callsiteTyper.context.unit, applyFSM, async.asyncSymbols.Async_await, Map.empty)

      atPos(asyncBody.pos)(ClassDef(NoMods, tpnme.stateMachine, Nil,
                                     gen.mkTemplate(parents, noSelfType, NoMods, List(Nil),
                                                     List(stateVar, resultVal, onComplete, tryGet, execContextVal, completeFailure, completeSuccess, getCompleted, applyFSM))))
    }

    val newStateMachine = ValDef(NoMods, nme.stateMachine, TypeTree(), Apply(Select(New(Ident(tpnme.stateMachine)), nme.CONSTRUCTOR), Nil))
    def execContextSelect = Select(Ident(nme.stateMachine), nme.execContext)

    // Use KeptPromise.onComplete to get the ball rolling.
    val futureUnit = gen.mkAttributedSelect(gen.mkAttributedStableRef(FutureClass.companionModule), Future_unit)

    // stateMachine.asInstanceOf[Function1[Try[Unit], Unit]
    // This cast is safe because we know that `def apply` does not consult its argument when `state == 0`.
    val castStateMachine = gen.mkCast(Ident(nme.stateMachine),
      definitions.functionType(appliedType(TryClass, definitions.UnitTpe :: Nil) :: Nil, definitions.UnitTpe))

    val stateMachineToFuture = Apply(Apply(TypeApply(Select(futureUnit, Future_onComplete), TypeTree(definitions.UnitTpe) :: Nil), castStateMachine :: Nil), execContextSelect :: Nil)

    val promToFuture = gen.mkCast(Select(Select(Ident(nme.stateMachine), nme.result), nme.future), appliedType(FutureClass, resultType))

    Block(List(execContextTempVal, stateMachine, newStateMachine, stateMachineToFuture), promToFuture)
  }
}
