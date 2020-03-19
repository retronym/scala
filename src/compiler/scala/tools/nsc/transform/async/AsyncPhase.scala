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
import scala.reflect.internal.Flags
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.AbstractFile

abstract class AsyncPhase extends Transform with TypingTransformers with AnfTransform with AsyncAnalysis with Lifter with LiveVariables {
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

  /**
   * Mark the given method as requiring an async transform.
   */
  final def markForAsyncTransform(pos: Position, method: DefDef, awaitMethod: Symbol,
                                  config: Map[String, AnyRef]): DefDef = {
    if (!settings.async)
      reporter.warning(pos, s"${settings.async.name} must be enabled for async transformation.")
    sourceFilesToTransform += pos.source
    val postAnfTransform = config.getOrElse("postAnfTransform", (x: Block) => x).asInstanceOf[Block => Block]
    val stateDiagram = config.getOrElse("stateDiagram", (sym: Symbol, tree: Tree) => None).asInstanceOf[(Symbol, Tree) => Option[String => Unit]]
    method.updateAttachment(new AsyncAttachment(awaitMethod, postAnfTransform, stateDiagram))
    deriveDefDef(method) { rhs =>
      Block(rhs.updateAttachment(SuppressPureExpressionWarning), Literal(Constant(())))
    }
  }

  protected object macroExpansion extends AsyncEarlyExpansion {
    val global: self.global.type = self.global
  }

  final class AsyncSymbols(val Async_async: Symbol, val Async_await: Symbol) {
    // Custom FutureSystems could add their own await method here.
    val awaits = mutable.HashSet(Async_await)
  }
  lazy val asyncSymbols: AsyncSymbols = {
    rootMirror.getPackageIfDefined("scala.async") match {
      case NoSymbol if settings.async =>
        val completer = new loaders.PackageLoader("async", platform.classPath)
        val packSym = loaders.enterPackage(definitions.ScalaPackageClass, "async", completer)


        val Async_module_completer = new loaders.SymbolLoader {
          override def sourcefile: Option[AbstractFile] = None
          override protected def doComplete(root: Symbol): Unit = {
            root.setInfo(ClassInfoType(Nil, newScope, root.moduleClass))
          }
          override protected def description: String = ""
        }
        val AsyncModule = packSym.moduleClass.newModule("Async")
        val moduleClass = AsyncModule.moduleClass
        moduleClass.setInfo(ClassInfoType(Nil, newScope, moduleClass))
        AsyncModule.setInfoAndEnter(TypeRef(packSym.thisType, moduleClass, Nil))

        def futureOfT(tparam: Symbol) = appliedType(rootMirror.getClassIfDefined("scala.concurrent.Future").tpeHK, tparam.tpeHK)

        val Async_async = AsyncModule.moduleClass.newMethodSymbol(nme.async, newFlags = Flags.MACRO)
        val Async_async_T = Async_async.newSyntheticTypeParam("T", 0L).setInfo(TypeBounds.empty)
        val Async_async_body = Async_async.newSyntheticValueParam(Async_async_T.tpeHK, "body")
        val Async_async_execContext = Async_async.newSyntheticValueParam(rootMirror.getClassIfDefined("scala.concurrent.ExecutionContext").tpeHK, "executionContext").setFlag(Flags.IMPLICIT)
        Async_async.setInfoAndEnter(PolyType(Async_async_T :: Nil, MethodType(Async_async_body :: Nil, MethodType(Async_async_execContext :: Nil, futureOfT(Async_async_T)))))

        val Async_await = AsyncModule.moduleClass.newMethodSymbol(nme.await)
        val Async_await_T = Async_await.newSyntheticTypeParam("T", 0L).setInfo(TypeBounds.empty)
        val Async_await_future = Async_await.newSyntheticValueParam(futureOfT(Async_await_T), "awaitable")
        Async_await.setInfoAndEnter(PolyType(Async_await_T :: Nil, MethodType(Async_await_future :: Nil, Async_await_T.tpeHK)))
        new AsyncSymbols(Async_async, Async_await)
      case _ =>
        val AsyncModule = rootMirror.getModuleIfDefined("scala.async.Async")
        val Async_async = AsyncModule.map(async => definitions.getDeclIfDefined(async, nme.async))
        val Async_await = AsyncModule.map(async => definitions.getDeclIfDefined(async, nme.await))
        new AsyncSymbols(Async_async, Async_await)
    }
  }

  override def newPhase(prev: scala.reflect.internal.Phase): StdPhase = {
    new Phase(prev) {
      override def init(): Unit = {
        asyncSymbols // force
      }
    }
  }
  import treeInfo.Applied
  def fastTrackEntry: (Symbol, PartialFunction[Applied, scala.reflect.macros.contexts.Context { val universe: self.global.type } => Tree]) =
    (asyncSymbols.Async_async, {
      // def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro ???
      case app@Applied(_, _, List(asyncBody :: Nil, execContext :: Nil)) =>
        c => c.global.async.macroExpansion.apply(c.callsiteTyper, asyncBody, execContext, asyncBody.tpe)
    })

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  final class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private lazy val liftableMap = new mutable.AnyRefMap[Symbol, (Symbol, List[Tree])]()

    override def transformUnit(unit: CompilationUnit): Unit = {
      if (settings.async) {
        if (sourceFilesToTransform.contains(unit.source)) super.transformUnit(unit)
        if (asyncSymbols.awaits.exists(_.isInitialized)) {
          unit.body.foreach {
            case tree: RefTree if tree.symbol != null && asyncSymbols.awaits.contains(tree.symbol) =>
              val sym = tree.symbol
              val msg = sym.compileTimeOnlyMessage.getOrElse(s"`${sym.decodedName}` must be enclosed in an `async` block")
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
          new UseFields(localTyper, cd.symbol, applySym, liftedSyms).transform(cd1)

        case dd: DefDef if dd.hasAttachment[AsyncAttachment] =>
          val asyncAttachment = dd.getAndRemoveAttachment[AsyncAttachment].get
          val asyncBody = (dd.rhs: @unchecked) match {
            case blk@Block(stats, Literal(Constant(()))) => treeCopy.Block(blk, stats.init, stats.last).setType(stats.last.tpe)
          }

          val saved = currentTransformState
          atOwner(dd, dd.symbol) {
            val trSym = dd.vparamss.head.head.symbol
            val saved = currentTransformState
            currentTransformState = new AsyncTransformState(asyncAttachment.awaitSymbol,
              asyncAttachment.postAnfTransform, asyncAttachment.stateDiagram, this, trSym, asyncBody.tpe)
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
      reportUnsupportedAwaits(asyncBody)

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
      val liftedSyms = liftedFields.map(_.symbol).toSet

      // Null out lifted fields become unreachable at each state.
      val nullOut = true
      if (nullOut) {
        for ((state, flds) <- fieldsToNullOut(asyncBlock.asyncStates, asyncBlock.asyncStates.last, liftedFields)) {
          val asyncState = asyncBlock.asyncStates.find(_.state == state).get
          asyncState.insertNullAssignments(flds.iterator)
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
          assignUnitType(treeCopy.Assign(tree, fieldSel(tree), transform(rhs.changeOwner(tree.symbol, currentOwner))))
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
}
