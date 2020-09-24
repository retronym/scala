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

import scala.collection.mutable
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.reflect.internal.util.SourceFile

abstract class AsyncPhase extends Transform with TypingTransformers with AnfTransform with Lifter with LiveVariables {
  self =>
  import global._

  private[async] var currentTransformState: AsyncTransformState = _
  private[async] val asyncNames = new AsyncNames[global.type](global)
  protected[async] val tracing = new Tracing

  val phaseName: String = "async"
  override def enabled: Boolean = settings.async

  private final case class AsyncAttachment(awaitSymbol: Symbol, postAnfTransform: Block => Block, stateDiagram: ((Symbol, Tree) => Option[String => Unit])) extends PlainAttachment

  // Optimization: avoid the transform altogether if there are no async blocks in a unit.
  private val sourceFilesToTransform = perRunCaches.newSet[SourceFile]()
  private val awaits: mutable.Set[Symbol] = perRunCaches.newSet[Symbol]()

  /**
   * Mark the given method as requiring an async transform.
   * Refer to documentation in the public API that forwards to this method in src/reflect/scala/reflect/api/Internals.scala
   */
  final def markForAsyncTransform(owner: Symbol, method: DefDef, awaitMethod: Symbol,
                                  config: Map[String, AnyRef]): DefDef = {
    val pos = owner.pos
    if (!settings.async)
      reporter.warning(pos, s"${settings.async.name} must be enabled for async transformation.")
    sourceFilesToTransform += pos.source
    val postAnfTransform = config.getOrElse("postAnfTransform", (x: Block) => x).asInstanceOf[Block => Block]
    val stateDiagram = config.getOrElse("stateDiagram", (sym: Symbol, tree: Tree) => None).asInstanceOf[(Symbol, Tree) => Option[String => Unit]]
    method.updateAttachment(new AsyncAttachment(awaitMethod, postAnfTransform, stateDiagram))
    // Wrap in `{ expr: Any }` to force value class boxing before calling `completeSuccess`, see test/async/run/value-class.scala
    deriveDefDef(method) { rhs =>
      Block(Apply(gen.mkAttributedRef(definitions.Predef_locally), rhs :: Nil), Literal(Constant(())))
    }.updateAttachment(ChangeOwnerAttachment(owner))
  }

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  private def compileTimeOnlyPrefix: String = "[async] "

  /** Should refchecks defer reporting `@compileTimeOnly` errors for `sym` and instead let this phase issue the warning
   *  if they survive the async tranform? */
  private[scala] def deferCompileTimeOnlyError(sym: Symbol): Boolean = settings.async && {
    awaits.contains(sym) || {
      val msg = sym.compileTimeOnlyMessage.getOrElse("")
      val shouldDefer =
        msg.startsWith(compileTimeOnlyPrefix) || (sym.name == nme.await) && msg.contains("must be enclosed") && sym.owner.info.member(nme.async) != NoSymbol
      if (shouldDefer) awaits += sym
      shouldDefer
    }
  }

  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  final class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private lazy val liftableMap = new mutable.AnyRefMap[Symbol, (Symbol, List[Tree])]()

    override def transformUnit(unit: CompilationUnit): Unit = {
      if (settings.async) {
        if (sourceFilesToTransform.contains(unit.source)) super.transformUnit(unit)
        if (awaits.exists(_.isInitialized)) {
          unit.body.foreach {
            case tree: RefTree if tree.symbol != null && awaits.contains(tree.symbol) =>
              val sym = tree.symbol
              val msg = sym.compileTimeOnlyMessage.getOrElse(s"`${sym.decodedName}` must be enclosed in an `async` block").stripPrefix(compileTimeOnlyPrefix)
              global.reporter.error(tree.pos, msg)
            case _ =>
          }
        }
      }
    }

    // Together, these transforms below target this tree shaps
    // {
    //    class $STATE_MACHINE extends ... {
    //      def $APPLY_METHOD(....) = {
    //      ...
    //      }.updateAttachment(AsyncAttachment(...))
    //    }
    // }
    //
    // The RHS of the method is transformed into a state machine with that transformation tailored by the
    // attached `FutureSystem`. Local val/var/def/class/object trees that are referred to from multiple states
    // are lifted into members of the enclosing class.
    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        case cd: ClassDef if liftableMap.contains(cd.symbol) =>
          val (applySym, liftedTrees) = liftableMap.remove(cd.symbol).get
          val liftedSyms = liftedTrees.iterator.map(_.symbol).toSet
          val cd1 = atOwner(cd.symbol) {
            deriveClassDef(cd)(impl => {
              deriveTemplate(impl)(liftedTrees ::: _)
            })
          }
          assert(localTyper.context.owner == cd.symbol.owner)
          val withFields = new UseFields(localTyper, cd.symbol, applySym, liftedSyms).transform(cd1)
          withFields

        case dd: DefDef if dd.hasAttachment[AsyncAttachment] =>
          val asyncAttachment = dd.attachments.get[AsyncAttachment].get
          val asyncBody = (dd.rhs: @unchecked) match {
            case blk@Block(Apply(qual, body :: Nil) :: Nil, Literal(Constant(()))) => body
          }

          atOwner(dd, dd.symbol) {
            val trSym = dd.vparamss.head.head.symbol
            val saved = currentTransformState
            currentTransformState = new AsyncTransformState(asyncAttachment.awaitSymbol,
              asyncAttachment.postAnfTransform, asyncAttachment.stateDiagram, this, trSym, asyncBody.tpe, asyncNames)
            try {
              val (newRhs, liftableFields) = asyncTransform(asyncBody)
              liftableMap(dd.symbol.owner) = (dd.symbol, liftableFields)
              deriveDefDef(dd)(_ => newRhs)
            } finally {
              currentTransformState = saved
            }
          }
        case tree =>
          tree
      }

    private def asyncTransform(asyncBody: Tree): (Tree, List[Tree]) = {
      val transformState = currentTransformState
      import transformState.applySym

      val asyncPos = asyncBody.pos

      // We mark whether each sub-tree of `asyncBody` that do or do not contain an await in thus pre-processing pass.
      // The ANF transform can then efficiently query this to selectively transform the tree.
      markContainsAwait(asyncBody)

      // Transform to A-normal form:
      //  - no await calls in qualifiers or arguments,
      //  - if/match only used in statement position.
      val anfTree: Block = transformState.postAnfTransform(new AnfTransformer(localTyper).apply(asyncBody))

      // The ANF transform re-parents some trees, so the previous traversal to mark ancestors of
      // await is no longer reliable. Clear previous results and run it again for use in the `buildAsyncBlock`.
      cleanupContainsAwaitAttachments(anfTree)
      markContainsAwait(anfTree)

      val asyncBlock = buildAsyncBlock(anfTree)

      val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

      // Null out lifted fields become unreachable at each state.
      val nullOut = true
      if (nullOut) {
        for ((state, (preNulls, postNulls)) <- fieldsToNullOut(asyncBlock.asyncStates, asyncBlock.asyncStates.last, liftedFields)) {
          val asyncState = asyncBlock.asyncStates.find(_.state == state).get
          if (asyncState.nextStates.nonEmpty)
            asyncState.insertNullAssignments(preNulls.iterator, postNulls.iterator)
        }
      }

      // Assemble the body of the apply method, which is dispactches on the current state id.
      val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler)

      // Logging
      if ((settings.debug.value && shouldLogAtThisPhase))
        logDiagnostics(anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
      // Offer async frontends a change to produce the .dot diagram
      transformState.dotDiagram(applySym, asyncBody).foreach(f => f(asyncBlock.toDot))

      cleanupContainsAwaitAttachments(applyBody)

      (applyBody, liftedFields)
    }

    // Adjust the tree to:
    //   - lifted local variables are entered into the scope of the state machine class
    //   - references to them are rewritten as referencs to the fields.
    //   - the rhs of ValDefs that initialize such fields is turned into an assignment to the field
    private class UseFields(initLocalTyper: analyzer.Typer, stateMachineClass: Symbol,
                            applySym: Symbol, liftedSyms: Set[Symbol]) extends explicitOuter.OuterPathTransformer(initLocalTyper) {
      private def fieldSel(tree: Tree) = {
        assert(currentOwner != NoSymbol)
        val outerOrThis = if (stateMachineClass == currentClass) gen.mkAttributedThis(stateMachineClass) else {
          // These references need to be selected from an outer reference, because explicitouter
          // has already run we must perform this transform explicitly here.
          tree.symbol.makeNotPrivate(tree.symbol.owner)
          outerPath(outerValue, currentClass.outerClass, stateMachineClass)
        }
        atPos(tree.pos)(Select(outerOrThis.setType(stateMachineClass.tpe), tree.symbol).setType(tree.symbol.tpe))
      }
      override def transform(tree: Tree): Tree = tree match {
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          val rhs1 = transform(rhs.changeOwner(tree.symbol, currentOwner))
          deriveTree(rhs1, definitions.UnitTpe)(t => treeCopy.Assign(rhs1, fieldSel(tree), adapt(t, tree.symbol.tpe)))
        case _: DefTree if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          EmptyTree
        case md: MemberDef =>
          if (currentOwner == stateMachineClass) {
            if (liftedSyms(tree.symbol)) {
              stateMachineClass.info.decls.enter(md.symbol)
              super.transform(tree)
            } else if (md.symbol == applySym || md.symbol == stateMachineClass) {
              super.transform(tree)
            } else tree
          } else super.transform(tree)
        case Assign(i @ Ident(name), rhs) if liftedSyms(i.symbol) =>
          treeCopy.Assign(tree, fieldSel(i), adapt(transform(rhs), i.symbol.tpe))
        case Ident(name) if liftedSyms(tree.symbol) =>
          fieldSel(tree).setType(tree.tpe)
        case _: TypeTree =>
          tree
        case _ =>
          super.transform(tree)
      }

      // Use localTyper to adapt () to BoxedUnit in `val ifRes: Object; if (cond) "" else ()`
      private def adapt(tree: Tree, pt: Type): Tree = localTyper.typed(tree, pt)
    }

    private def logDiagnostics(anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
      val pos = currentTransformState.applySym.pos
      val location = try pos.source.path catch { case _: UnsupportedOperationException => pos.toString }
      inform(s"In file '$location':")
      inform(s"ANF transform expands to:\n $anfTree")
      states foreach (s => inform(s))
      inform("===== DOT =====")
      inform(block.toDot)
    }
  }

  /**
   * Utility to post-process a FSM translated class that requires no storage to capture continuations into an
   * equivalent lambda-backed representation that has less overhead in the generated class files.
   *
   * Given an input tree of the form:
   *
   * ```
   * {
   *   class stateMachine$async extends scala.tools.nsc.async.CustomFutureStateMachine {
   *     def <init>($outer: C): stateMachine$async = {
   *       stateMachine$async.super.<init>();
   *       ()
   *     };
   *     def fsm(tr: scala.util.Either): Object = while$(){
   *       try {
   *         stateMachine$async.this.state() match {
   *           case 0 => {
   *             val awaitable$async: scala.tools.nsc.async.CustomFuture = scala.tools.nsc.async.CustomFuture._successful(Util.id("a"));
   *             tr = stateMachine$async.this.getCompleted(awaitable$async);
   *             stateMachine$async.this.state_=(1);
   *             ...
   *       };
   *       while$()
   *     };
   *     <synthetic> <paramaccessor> <artifact> private[this] val $outer: C = _;
   *     <synthetic> <stable> <artifact> def $outer(): C = stateMachine$async.this.$outer
   *   };
   *   new stateMachine$async(C.this).start()
   * }
   * ```
   *
   * Convert to:
   * ```
   * {
   *   val self: tools.nsc.async.GenericCustomFutureStateMachine = new tools.nsc.async.GenericCustomFutureStateMachine();
   *   self.setFsmAndStart({
   *     <artifact> def fsm(tr: scala.util.Either): Object = while$(){
   *       try {
   *         self.state() match {
   *           case 0 => {
   *             val awaitable$async: scala.tools.nsc.async.CustomFuture = scala.tools.nsc.async.CustomFuture._successful(Util.id("a"));
   *             tr = self.getCompleted(awaitable$async);
   *             self.state_=(1);
   *             ...
   *       };
   *       while$()
   *     };
   *     ((tr$async: scala.util.Either) => fsm(tr$async))
   *   })
   * }
   * ```
   * @param currentOwner    The method containing the FSM translated code
   * @param localTyper      The local typer of the calling typing transform focussed on `currentOwner`
   * @param cd              The anonymous class definition
   * @param constructorArgs The arguments to the instantation of this class. Must either be empty or a single outer argument.
   * @param selfValDef      A typed tree of the form `val someName: StateMachine = ... `. `this` references to the elimimated
   *                        class will be substituted with references to this val.
   * @param bindLambda      A function that will be given the `self` symbol and the lambda equivalent of the FSM method,
   *                        which creates the expression that binds the lambda to the FSM's behaviour.
   * @return                A block with the new form of the FSM expression.
   */
  def lambdafy(currentOwner: Symbol, localTyper: global.analyzer.Typer, cd: ClassDef, constructorArgs: List[Tree],
               selfValDef: Tree, bindLambda: (Symbol, Tree) => Tree) = {
    val fsmMethod: DefDef = cd.impl.body.collectFirst {
      case dd: DefDef if dd.hasAttachment[AsyncAttachment] =>
        dd
    }.get
    val pos = fsmMethod.pos
    val self = selfValDef.symbol
    val classToEliminate = fsmMethod.symbol.owner
    fsmMethod.symbol.owner == currentOwner
    fsmMethod.symbol.updateAttachment(DelambdafyTarget).setFlag(Flag.ARTIFACT)
    fsmMethod.symbol.modifyInfo {
      case mt: MethodType => copyMethodType(mt, mt.params, definitions.ObjectTpe)
    }
    fsmMethod.updateAttachment(DelambdafyTarget)
    val funSymbol = currentOwner.newAnonymousFunctionClass().setInfo(typeOf[Object => Object])
    val funvparam = funSymbol.newValueParameter(nme.tr).setInfo(fsmMethod.symbol.firstParam.info)
    object deouter extends Transformer {
      def outerOuter = constructorArgs.head.duplicate
      def apply(t: Tree) =
        classToEliminate.primaryConstructor.paramss match {
          case List(Nil) =>
            t
          case List(List(outer)) =>
            outerField = classToEliminate.info.decl(nme.OUTER_LOCAL)
            outerAccessor = classToEliminate.info.decls.find(_.isOuterAccessor).get
            val tree1  = transform(t)
            val outerOuter = outerAccessor.info.resultType.typeSymbol
            val tree2 = tree1.substituteSymbols(classToEliminate :: Nil, outerOuter :: Nil)
            tree2
        }
      var outerField: Symbol = NoSymbol
      var outerAccessor: Symbol = NoSymbol
      override def transform(tree: Tree): Tree = tree match {
        case sel: Select if sel.symbol == outerField =>
          assert(currentOwner.enclClass == classToEliminate)
          constructorArgs.head.duplicate
        case Apply(sel: Select, Nil) if sel.symbol == outerAccessor =>
          assert(currentOwner.enclClass != classToEliminate)
          super.transform(sel.qualifier).setType(tree.tpe)
        case vd: ValDef if vd.name == nme.THIS =>
          vd.symbol.setName(currentUnit.freshTermName(vd.name.toString))
          vd.substituteSymbols(classToEliminate :: Nil, selfValDef.symbol.info.typeSymbol :: Nil)
        case Apply(qual, args) if tree.symbol.isPrimaryConstructor =>
          val args1 = map2Conserve(args, qual.symbol.info.params) {
            (arg, param) =>
              if (param.name == nme.OUTER_ARG && param.info.typeSymbol == classToEliminate) {
                if (currentOwner.enclClass == classToEliminate)
                  outerOuter
                else
                  arg
              } else arg
          }
          super.transform(treeCopy.Apply(tree, qual, args1))
        case _ => super.transform(tree)
      }
    }
    deouter.currentOwner = currentOwner
    object boxReturn extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Return(Literal(Constant(()))) if tree.symbol == fsmMethod.symbol =>
          treeCopy.Return(tree, literalBoxedUnit).setType(definitions.BoxedUnitTpe)
        case _ =>
          super.transform(tree)
      }
    }
    boxReturn.currentOwner = currentOwner

    val dd1         = deriveDefDef(fsmMethod)(rhs => Block(rhs :: Nil, literalBoxedUnit)).substituteThis(classToEliminate, Ident(selfValDef.symbol).setType(selfValDef.symbol.tpeHK))
    val dd2         = deouter(dd1)
    val dd3        = boxReturn.transform(dd2)
    val lambdaBlock = Block(dd3.changeOwner(fsmMethod.symbol.owner, currentOwner) :: Nil,
                            Function(ValDef(funvparam) :: Nil, gen.mkForwarder(gen.mkAttributedRef(fsmMethod.symbol), List(List(funvparam)))))
    val blk         = Block(selfValDef :: Nil, bindLambda(self, lambdaBlock))
    localTyper.typedPos(pos)(blk)
  }
}
