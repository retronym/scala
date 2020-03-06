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

import java.util.function.IntUnaryOperator

import user.FutureSystem
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.existentials

trait ExprBuilder extends TransformUtils {
  import global._

  def tryAny = transformType(currentTransformState.ops.tryType(definitions.AnyTpe))

  private def stateAssigner  = currentTransformState.stateAssigner
  private def labelDefStates = currentTransformState.labelDefStates

  trait AsyncState {
    def state: Int

    def nextStates: Array[Int]

    def mkHandlerCaseForState[T]: CaseDef

    def isResumption: Option[Awaitable] = None

    protected def resumptionHandler: Option[Tree] = isResumption.map { awaitable =>
      val futureSystem = currentTransformState.futureSystem
      val futureSystemOps = futureSystem.mkOps(global)
      def tryyReference = Ident(currentTransformState.symLookup.applyTrParam)
      val tryyGet = futureSystemOps.tryyGet[Any](tryyReference)

      val assignTryGet = Assign(Ident(awaitable.resultName), tryyGet)
      if (futureSystem.emitTryCatch) {
        If(futureSystemOps.tryyIsFailure(tryyReference),
          Block(toList(futureSystemOps.completeProm[AnyRef](
            currentTransformState.symLookup.selectResult,
            tryyReference)),
            Return(literalUnit)),
          assignTryGet
        )
      } else {
        assignTryGet
      }
    }

    var stats: List[Tree]

    def treeThenStats(tree: Tree): List[Tree] =
      adaptToUnitIgnoringNothing(tree :: stats) :: Nil

    final def allStats: List[Tree] = this match {
      case a: AsyncStateWithAwait => treeThenStats(a.awaitable.resultValDef)
      case _ => stats
    }

    final def body: Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
      case Nil => literalUnit
    }
  }

  /** A sequence of statements that concludes with a unconditional transition to `nextState` */
  final class SimpleAsyncState(var stats: List[Tree], val state: Int, nextState: Int, val nextStates: Array[Int], override val isResumption: Option[Awaitable])
    extends AsyncState {

    def mkHandlerCaseForState[T]: CaseDef = {
      mkHandlerCase(state, treeThenStats(mkStateTree(nextState)))
    }

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  /** A sequence of statements with a conditional transition to the next state, which will represent
    * a branch of an `if` or a `match`.
    */
  final class AsyncStateWithoutAwait(var stats: List[Tree], val state: Int, val nextStates: Array[Int], override val isResumption: Option[Awaitable]) extends AsyncState {
    override def mkHandlerCaseForState[T]: CaseDef =
      mkHandlerCase(state, resumptionHandler.toList ++ stats)

    override val toString: String =
      s"AsyncStateWithoutAwait #$state, nextStates = ${nextStates.toList}"
  }

  /** A sequence of statements that concludes with an `await` call. The `onComplete`
    * handler will unconditionally transition to `nextState`.
    */
  final class AsyncStateWithAwait(var stats: List[Tree], val state: Int, val nextState: Int, val nextStates: Array[Int],
                                  val awaitable: Awaitable, override val isResumption: Option[Awaitable])
    extends AsyncState {

    override def mkHandlerCaseForState[T]: CaseDef = {
      val futureSystem = currentTransformState.futureSystem
      val futureSystemOps = futureSystem.mkOps(global)
      val fun = This(tpnme.EMPTY)
      val symLookup = currentTransformState.symLookup
      val tryGetOrCallOnComplete: List[Tree] =
        if (futureSystemOps.continueCompletedFutureOnSameThread) {
          val tempAwaitableSym = symLookup.applyTrParam.owner.newTermSymbol(nme.awaitable).setInfo(awaitable.expr.tpe)
          val initAwaitableTemp = ValDef(tempAwaitableSym, awaitable.expr)
          val tempCompletedSym = symLookup.applyTrParam.owner.newTermSymbol(nme.completed).setInfo(tryAny)
          val initTempCompleted = ValDef(tempCompletedSym, futureSystemOps.getCompleted[Any](Ident(tempAwaitableSym)))
          val null_ne = Select(Literal(Constant(null)), TermName("ne"))
          val callOnComplete = futureSystemOps.onComplete[Any, Unit](Ident(tempAwaitableSym), fun, Ident(nme.execContext), definitions.AnyTpe)
          val ifTree =
            If(Apply(null_ne, Ident(tempCompletedSym) :: Nil),
              adaptToUnit(ifIsFailureTree[T](Ident(tempCompletedSym)) :: Nil),
              Block(toList(callOnComplete), Return(literalUnit)))
          initAwaitableTemp :: initTempCompleted :: ifTree :: Nil
        } else {
          val callOnComplete = futureSystemOps.onComplete[Any, Unit](awaitable.expr, fun, Ident(nme.execContext), definitions.AnyTpe)
          toList(callOnComplete) ::: Return(literalUnit) :: Nil
        }
      mkHandlerCase(state, resumptionHandler.toList ++ stats ++ List(mkStateTree(nextState)) ++ tryGetOrCallOnComplete)
    }

    /* if (tr.isFailure)
     *   result.complete(tr.asInstanceOf[Try[T]])
     * else {
     *   <resultName> = tr.get.asInstanceOf[<resultType>]
     *   <nextState>
     *   <mkResumeApply>
     * }
     */
    def ifIsFailureTree[T](tryReference: => Tree) = {
      val futureSystem = currentTransformState.futureSystem
      val futureSystemOps = futureSystem.mkOps(global)

      assert(isPastErasure)
      val tryyGet = futureSystemOps.tryyGet[Any](tryReference)

      val getAndUpdateState = Block(List(Assign(Ident(awaitable.resultName), tryyGet)), mkStateTree(nextState))
      if (futureSystem.emitTryCatch) {
        If(futureSystemOps.tryyIsFailure(tryReference),
          Block(toList(futureSystemOps.completeProm[T](
            currentTransformState.symLookup.selectResult,
            tryReference)),
            Return(literalUnit)),
          getAndUpdateState
        )
      } else {
        getAndUpdateState
      }
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async expression.
   */
  private final class AsyncStateBuilder(state: Int, val isResumption: Option[Awaitable] = None) {
    /* Statements preceding an await call. */
    private val stats                      = ListBuffer[Tree]()
    /** The state of the target of a LabelDef application (while loop jump) */
    private var nextJumpState: Option[Int] = None
    private var nextJumpSymbol: Symbol = NoSymbol
    def effectiveNextState(nextState: Int) =
      nextJumpState.orElse(if (nextJumpSymbol == NoSymbol) None
                           else Some(stateIdForLabel(nextJumpSymbol))).getOrElse(nextState)
    val caseJumpStates: StateSet = new StateSet

    def +=(stat: Tree): this.type = {

      // Allow `()` (occurs in do/while)
      assert(isLiteralUnit(stat) || nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")

      stat match {
        case Apply(fun, args) if isLabel(fun.symbol) && fun.symbol != currentTransformState.symLookup.whileLabel =>
          // labelDefStates belongs to the current ExprBuilder
          labelDefStates get fun.symbol match {
            case opt@Some(nextState) =>
              // A backward jump
              nextJumpState = opt // re-use object
              nextJumpSymbol = fun.symbol
            case None =>
              // We haven't the corresponding LabelDef, this is a forward jump
              nextJumpSymbol = fun.symbol
          }
        case _               =>
          val trans = new replaceJumpsWithStateTransitions( caseJumpStates)
          stats += trans.transformAtOwner(currentTransformState.localTyper.context.owner, stat)
      }
      this
    }

    def resultWithAwait(awaitable: Awaitable,
                        nextState: Int): AsyncState = {
      val nextState1 = effectiveNextState(nextState)
      val allNextStates = nextState1 +: caseJumpStates.iterator.map(_.toInt).toArray.distinct
      new AsyncStateWithAwait(stats.toList, state, nextState1, allNextStates, awaitable, isResumption)
    }

    def resultSimple(nextState: Int): AsyncState = {
      val nextState1 = effectiveNextState(nextState)
      val allNextStates = nextState1 +: caseJumpStates.iterator.map(_.toInt).toArray.distinct
      new SimpleAsyncState(stats.toList, state, nextState1, allNextStates, isResumption)
    }

    def isEmpty: Boolean = stats.isEmpty

    def resultWithIf(condTree: Tree, thenState: Int, elseState: Int): AsyncState = {
      assert(caseJumpStates.isEmpty)
      def mkBranch(state: Int) = mkStateTree(state)
      this += If(condTree, mkBranch(thenState), mkBranch(elseState))
      new AsyncStateWithoutAwait(stats.toList, state, Array(thenState, elseState), isResumption)
    }

    /**
     * Build `AsyncState` ending with a match expression.
     *
     * The cases of the match simply resume at the state of their corresponding right-hand side.
     *
     * @param scrutTree       tree of the scrutinee
     * @param cases           list of case definitions
     * @param caseStates      starting state of the right-hand side of the each case
     * @return                an `AsyncState` representing the match expression
     */
    def resultWithMatch(scrutTree: Tree, cases: List[CaseDef], caseStates: Array[Int]): AsyncState = {
      // 1. build list of changed cases
      val newCases = for ((cas, num) <- cases.zipWithIndex) yield cas match {
        case CaseDef(pat, guard, rhs) =>
          val bindAssigns = rhs.children.takeWhile(isSyntheticBindVal)
          CaseDef(pat, guard, Block(bindAssigns, mkStateTree(caseStates(num))))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(scrutTree, newCases)
      new AsyncStateWithoutAwait(stats.toList, state, caseStates, isResumption)
    }

    def resultWithLabel(startLabelState: Int): AsyncState = {
      this += mkStateTree(startLabelState)
      new AsyncStateWithoutAwait(stats.toList, state, startLabelState +: caseJumpStates.iterator.map(_.toInt).toArray, isResumption)
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait"
    }
  }

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   */
  final private class AsyncBlockBuilder(stats: List[Tree], expr: Tree, startState: Int, endState: Int) {
    val asyncStates = ListBuffer[AsyncState]()

    var stateBuilder = new AsyncStateBuilder(startState)
    var currState    = startState

    def checkForUnsupportedAwait(tree: Tree) = if (containsAwait(tree))
      global.reporter.error(tree.pos, "await must not be used in this position")

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState)
    }

    def nextState() = stateAssigner.nextState()

    case class PatternIndex(caseByMatchEnd: collection.Map[Symbol, PatternIndexEntry], matchEndByCase: Map[Symbol, Symbol]) {

      for ((_, entry) <- caseByMatchEnd) {
        entry.subsequentCasesOfAwait = entry.containsAwait.flatMap(a => entry.subsequentCases.getOrElse(a, Nil)).toSet
        (entry.cases -- entry.subsequentCasesOfAwait).foreach(labelDefStates.remove(_))
      }

      def isTerminalCase(caseOrMatchEndSym: Symbol): Boolean = {
        val cases = caseByMatchEnd(matchEndByCase.getOrElse(caseOrMatchEndSym, caseOrMatchEndSym)).cases
        cases.last == caseOrMatchEndSym
      }
      def matchContainsAwait(caseOrMatchEndSym: Symbol): Boolean = {
        matchEndByCase.get(caseOrMatchEndSym) match {
          case Some(indexedMatchEnd) =>
            caseByMatchEnd(indexedMatchEnd).containsAwait.nonEmpty
          case None =>
            caseByMatchEnd.get(caseOrMatchEndSym) match {
              case Some(indexedMatchEnd) => indexedMatchEnd.containsAwait.nonEmpty
              case _ => false
            }
        }
      }
    }

    class PatternIndexEntry {
      val cases = mutable.ListBuffer[Symbol]()
      val subsequentCases =  mutable.HashMap[Symbol, mutable.HashSet[Symbol]]()
      val containsAwait = mutable.HashSet[Symbol]()

      var subsequentCasesOfAwait: Set[Symbol] = null
    }
    val patternIndex: PatternIndex = {
      val trees = (stats :+ expr).filterNot(isLiteralUnit).toArray
      val caseByMatchEnd = mutable.HashMap[Symbol, PatternIndexEntry]()
      var currentMatchEnd: Symbol = null
      for (tree <- trees.reverseIterator) {
        tree match {
          case LabelDef(name, _, _) if name.startsWith("matchEnd") =>
            currentMatchEnd = tree.symbol
          case ld @ LabelDef(name, _, _) if name.startsWith("case") =>
            if (currentMatchEnd != null) {
              val entry = caseByMatchEnd.getOrElseUpdate(currentMatchEnd, new PatternIndexEntry())
              entry.cases.prepend(tree.symbol)
              if (containsAwait(tree)) {
                entry.containsAwait.add(ld.symbol)
              }
              tree foreach {
                case Apply(fun, _) if isLabel(fun.symbol) && caseByMatchEnd.get(currentMatchEnd).exists(_.cases.contains(fun.symbol)) =>
                  entry.subsequentCases.getOrElseUpdate(ld.symbol, mutable.HashSet()) += fun.symbol
                case _ =>
              }
            }
          case _ =>
        }
      }
      for ((matchEnd, entry) <- caseByMatchEnd) {
        if (entry.containsAwait.nonEmpty) {
          for (caseDef <- entry.cases) {
            labelDefStates(caseDef) = stateIdForLabel(caseDef)
          }
          labelDefStates(matchEnd) = stateIdForLabel(matchEnd)
        }
      }
      val matchEndByCase = caseByMatchEnd.iterator.flatMap {
        case (matchEnd, entry) => entry.cases.map(c => (c, matchEnd))
      }.toMap

      PatternIndex(caseByMatchEnd, matchEndByCase)
    }

    // `while(await(x))` ... or `do { await(x); ... } while(...)` contain an `If` that loops;
    // we must break that `If` into states so that it convert the label jump into a state machine
    // transition
    private def containsForeignLabelJump(t: Tree): Boolean = {
      val labelDefs = t.collect { case ld: LabelDef => ld.symbol }.toSet
      t.exists {
        case rt: RefTree => rt.symbol != null && isLabel(rt.symbol) && !(labelDefs contains rt.symbol) && !rt.symbol.name.startsWith("case") && rt.symbol != currentTransformState.symLookup.whileLabel
        case _ => false
      }
    }

    // unwrap Block(t :: Nil, scala.runtime.BoxedUnit.UNIT) -- erasure will add the expr when await had type Unit
    object UnwrapBoxedUnit {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case Block(t :: Nil, unit) if isLiteralUnit(unit) => Some(t) // is really only going to be BoxedUnit, but hey
        case t => Some(t)
      }
    }
    // populate asyncStates
    def add(stat: Tree, afterState: Option[Int] = None): Unit = stat match {
      // the val name = await(..) pattern
      case vd @ ValDef(mods, name, tpt, UnwrapBoxedUnit(Apply(fun, arg :: Nil))) if currentTransformState.ops.isAwait(fun) =>
        val afterAwaitState = afterState.getOrElse(nextState())
        val awaitable = Awaitable(arg.changeOwner(vd.symbol, vd.symbol.owner), stat.symbol, tpt.tpe, vd)
        asyncStates += stateBuilder.resultWithAwait(awaitable, afterAwaitState) // complete with await
        currState = afterAwaitState
        stateBuilder = new AsyncStateBuilder(currState, isResumption = Some(awaitable))

      case If(cond, thenp, elsep) if containsAwait(stat) || containsForeignLabelJump(stat) =>
        checkForUnsupportedAwait(cond)

        val thenStartState = nextState()
        val elseStartState = nextState()
        val afterIfState = afterState.getOrElse(nextState())

        // the two Int arguments are the start state of the then branch and the else branch, respectively
        asyncStates += stateBuilder.resultWithIf(cond, thenStartState, elseStartState)

        List((thenp, thenStartState), (elsep, elseStartState)) foreach {
          case (branchTree, state) =>
            val builder = nestedBlockBuilder(branchTree, state, afterIfState)
            asyncStates ++= builder.asyncStates
        }

        currState = afterIfState
        stateBuilder = new AsyncStateBuilder(currState)

      case Match(scrutinee, cases) if containsAwait(stat) =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = new Array[Int](cases.length)
        java.util.Arrays.setAll(caseStates, new IntUnaryOperator {
          override def applyAsInt(operand: Int): Int = nextState()
        })
        val afterMatchState = afterState.getOrElse(nextState())

        asyncStates += stateBuilder.resultWithMatch(scrutinee, cases, caseStates)

        for ((cas, num) <- cases.zipWithIndex) {
          val (stats, expr) = statsAndExpr(cas.body)
          val stats1 = stats.dropWhile(isSyntheticBindVal)
          val builder = nestedBlockBuilder(Block(stats1, expr), caseStates(num), afterMatchState)
          asyncStates ++= builder.asyncStates
        }

        currState = afterMatchState
        stateBuilder = new AsyncStateBuilder(currState)
      case ld @ LabelDef(name, params, rhs) =>
        if (patternIndex.matchContainsAwait(ld.symbol)) {
          val needsState = labelDefStates.contains(ld.symbol) || patternIndex.isTerminalCase(ld.symbol)
          if (needsState) {
            if (!stateBuilder.isEmpty) {
              val startLabelState = stateIdForLabel(ld.symbol)
              currState = startLabelState
              asyncStates += stateBuilder.resultSimple(startLabelState)
            }
            stateBuilder = new AsyncStateBuilder(currState)
          }
          if (containsAwait(rhs)) {
            stateBuilder += treeCopy.LabelDef(ld, ld.name, ld.params, literalUnit)
            add(rhs)
          } else if (ld.symbol.name.startsWith("matchEnd")) {
            currState = stateIdForLabel(ld.symbol)
            stateBuilder = new AsyncStateBuilder(currState)
          } else if (patternIndex.isTerminalCase(ld.symbol)) {
            checkForUnsupportedAwait(stat)
            stateBuilder += stat
          } else {
            stateBuilder += stat
          }
        } else {
          checkForUnsupportedAwait(stat)
          stateBuilder += stat
        }
      case b @ Block(stats, expr) =>
        for (stat <- stats) add(stat)
        add(expr, afterState = Some(endState))
      case _ =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    for (stat <- (stats :+ expr)) add(stat)
    val lastState = stateBuilder.resultSimple(endState)
    asyncStates += lastState
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler[T]: Tree

    def toDot: String
  }


  case class SymLookup(stateMachineClass: Symbol, applyTrParam: Symbol) {
    def stateMachineMember(name: TermName): Symbol = {
      stateMachineClass.info.member(name)
    }
    def memberRef(name: TermName): Tree =
      gen.mkAttributedRef(stateMachineClass.typeConstructor, stateMachineMember(name))
    def memberRef(sym: Symbol): Tree =
      gen.mkAttributedRef(stateMachineClass.typeConstructor, sym)

    lazy val stateGetter: Symbol = stateMachineMember(nme.state)
    lazy val stateSetter: Symbol = stateGetter.setterIn(stateGetter.owner)

    def selectResult = applyNilAfterUncurry(memberRef(nme.result))
    def applyMethod: Symbol = applyTrParam.owner
    val whileLabel: Symbol = applyMethod.newLabel(name.fresh(nme.WHILE_PREFIX)).setInfo(MethodType(Nil, definitions.UnitTpe))
  }

  private lazy val NonFatalClass = rootMirror.staticModule("scala.util.control.NonFatal")
  private lazy val ThrowableClass = rootMirror.staticClass("java.lang.Throwable")

  /**
   * Uses `AsyncBlockBuilder` to create an instance of `AsyncBlock`.
   *
   * @param  block      a `Block` tree in ANF
   * @return            an `AsyncBlock`
   */
  def buildAsyncBlock(block: Block): AsyncBlock = {
    val Block(stats, expr) = block
    val startState = stateAssigner.nextState()
    val endState = Int.MaxValue

    val blockBuilder = new AsyncBlockBuilder(stats, expr, startState, endState)

    new AsyncBlock {
      val switchIds = mutable.AnyRefMap[Integer, Integer]()

      // render with http://graphviz.it/#/new
      def toDot: String = {
        val states = asyncStates
        def toHtmlLabel(label: String, preText: String, builder: StringBuilder): Unit = {
          val br = "<br align=\"left\"/>"
          builder.append("<b>").append(label).append("</b>").append("<br/>")
          builder.append("<font face=\"Courier\">")
          preText.split("\n").foreach {
            (line: String) =>
              builder.append(br)
              // TODO Wrap with CDATA instead?
              builder.append(line.replaceAllLiterally("&", "&amp;").replaceAllLiterally("\"", "&quot;").replaceAllLiterally("<", "&lt;").replaceAllLiterally(">", "&gt;").replaceAllLiterally(" ", "&nbsp;"))
          }
          builder.append(br)
          builder.append("</font>")
        }
        val dotBuilder = new StringBuilder()
        dotBuilder.append("digraph {\n")
        def stateLabel(s: Int) = {
          if (s == 0) "INITIAL" else if (s == Int.MaxValue) "TERMINAL" else (if (compactStates) switchIds.getOrElse[Integer](s, s) else s).toString
        }
        val length = states.size
        for ((state, i) <- asyncStates.zipWithIndex) {
          dotBuilder.append(s"""${stateLabel(state.state)} [label=""").append("<")
          def show(t: Tree): String = {
            (t match {
              case Block(stats, expr) => stats ::: expr :: Nil
              case t => t :: Nil
            }).iterator.map(t => global.show(t)).mkString("\n")
          }
          if (i != length - 1) {
            val CaseDef(_, _, body) = state.mkHandlerCaseForState
            toHtmlLabel(stateLabel(state.state), show(compactStateTransform.transform(body)), dotBuilder)
          } else {
            toHtmlLabel(stateLabel(state.state), state.allStats.map(show(_)).mkString("\n"), dotBuilder)
          }
          dotBuilder.append("> ]\n")
        }
        for (state <- states) {
          for (succ <- state.nextStates) {
            val style = state match {
              case s: AsyncStateWithAwait if s.nextState == succ => "[style=dashed color=red]"
              case _ => ""
            }
            dotBuilder.append(s"""${stateLabel(state.state)} -> ${stateLabel(succ)} $style""")
            dotBuilder.append("\n")
          }
        }
        dotBuilder.append("}\n")
        dotBuilder.toString
      }

      lazy val asyncStates: List[AsyncState] = filterStates

      def filterStates = if (compactStates) {
        val all = blockBuilder.asyncStates.toList
        val (initial :: rest) = all
        val map = all.iterator.map(x => (x.state, x)).toMap
        val seen = mutable.HashSet[Int]()
        def loop(state: AsyncState): Unit = {
          seen.add(state.state)
          for (i <- state.nextStates) {
            if (i != Int.MaxValue && !seen.contains(i)) {
              map.get(i) match {
                case Some(i) => loop(i)
                case None => throw new NoSuchElementException(s"Unable to find state: $i in $map")
              }
            }
          }
        }
        loop(initial)
        val live = rest.filter(state => seen(state.state))
        var nextSwitchId = 0
        (initial :: live).foreach { state =>
          val switchId = nextSwitchId
          switchIds(state.state) = switchId
          nextSwitchId += 1
        }
        initial :: live
      } else blockBuilder.asyncStates.toList

      def mkCombinedHandlerCases[T]: List[CaseDef] = {
        val futureSystem = currentTransformState.futureSystem
        val futureSystemOps = futureSystem.mkOps(global)

        val caseForLastState: CaseDef = {
          val lastState = asyncStates.last
          val lastStateBody = lastState.body
          val rhs = futureSystemOps.completeWithSuccess(
            currentTransformState.symLookup.selectResult, lastStateBody, definitions.AnyTpe)
          mkHandlerCase(lastState.state, Block(rhs, Return(literalUnit)))
        }
        asyncStates match {
          case s :: Nil =>
            List(caseForLastState)
          case _        =>
            val initCases = for (state <- asyncStates.init) yield state.mkHandlerCaseForState[T]
            initCases :+ caseForLastState
        }
      }

      val initStates = asyncStates.init

      /**
       * Builds the definition of the `resume` method.
       *
       * The resulting tree has the following shape:
       *
       *       try {
       *         state match {
       *           case 0 => {
       *             f11 = exprReturningFuture
       *             f11.onComplete(onCompleteHandler)(context)
       *           }
       *           ...
       *         }
       *       } catch {
       *         case NonFatal(t) => result.failure(t)
       *       }
       */
      private def resumeFunTree[T]: Tree = {
        val futureSystem = currentTransformState.futureSystem
        val futureSystemOps = futureSystem.mkOps(global)

        val symLookup = currentTransformState.symLookup
        def stateMemberRef = gen.mkApplyIfNeeded(symLookup.memberRef(symLookup.stateGetter))
        val body =
          Match(stateMemberRef,
                 mkCombinedHandlerCases[T] ++
                 List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Apply(Select(New(Ident(IllegalStateExceptionClass)), termNames.CONSTRUCTOR), List(gen.mkMethodCall(definitions.StringModule.info.member(nme.valueOf), stateMemberRef :: Nil)))))))

        val body1 = compactStates(body)

        maybeTry(currentTransformState.futureSystem.emitTryCatch)(
          body1,
          List(
            CaseDef(
            Bind(nme.t, Typed(Ident(nme.WILDCARD), Ident(ThrowableClass))),
            EmptyTree, {
              val branchTrue = {
                val t = Ident(nme.t)
                val complete = futureSystemOps.completeProm[T](
                  symLookup.selectResult, futureSystemOps.tryyFailure[T](t))
                Block(toList(complete), Return(literalUnit))
              }
              If(Apply(Ident(NonFatalClass), List(Ident(nme.t))), branchTrue, Throw(Ident(nme.t)))
                branchTrue
            })), EmptyTree)
      }
      private def compactStates = true

      private val compactStateTransform = new Transformer {
        val symLookup = currentTransformState.symLookup
        override def transform(tree: Tree): Tree = tree match {
          case as @ Apply(qual: Select, (lit @ Literal(Constant(i: Integer))) :: Nil) if qual.symbol == symLookup.stateSetter && compactStates =>
            val replacement = switchIds(i)
            treeCopy.Apply(tree, qual, treeCopy.Literal(lit, Constant(replacement)):: Nil)
          case _: Match | _: CaseDef | _: Block | _: If | _: LabelDef =>
            super.transform(tree)
          case _ => tree
        }
      }

      private def compactStates(m: Match): Tree = if (!compactStates) m else {
        val casesAndReplacementIds: List[(Integer, CaseDef)] = m.cases.map {
          case cd @ CaseDef(lit @ Literal(Constant(i: Integer)), EmptyTree, rhs) =>
            val replacement = switchIds(i)
            val rhs1 = compactStateTransform.transform(rhs)
            (replacement, treeCopy.CaseDef(cd, treeCopy.Literal(lit, Constant(replacement)), EmptyTree, rhs1))
          case cd =>
            (Int.box(Integer.MAX_VALUE), cd)
        }
        val cases1: List[CaseDef] = casesAndReplacementIds.sortBy(_._1).map(_._2)
        treeCopy.Match(m, m.selector, cases1)
      }

      def forever(t: Tree): Tree = {
        val symLookup = currentTransformState.symLookup
        LabelDef(symLookup.whileLabel, Nil, Block(toList(t), Apply(Ident(symLookup.whileLabel), Nil)))
      }

      /**
       * Builds a `match` expression used as an onComplete handler, wrapped in a while(true) loop.
       */
      def onCompleteHandler[T]: Tree = {
        forever {
          adaptToUnit(toList(resumeFunTree))
        }
      }
    }
  }

  private def isSyntheticBindVal(tree: Tree) = tree match {
    case vd@ValDef(_, lname, _, Ident(rname)) => vd.symbol.attachments.contains[SyntheticBindVal.type]
    case _                                    => false
  }

  case class Awaitable(expr: Tree, resultName: Symbol, resultType: Type, resultValDef: ValDef)

  private def mkStateTree(nextState: Int): Tree = {
    val symLookup = currentTransformState.symLookup
    val callSetter = Apply(symLookup.memberRef(symLookup.stateSetter), Literal(Constant(nextState)) :: Nil)
    val debug = false
    if (debug) {
      Block(
        callSetter :: Nil,
        gen.mkMethodCall(definitions.PredefModule.info.member(TermName("println")), currentTransformState.localTyper.typed(gen.mkApplyIfNeeded(symLookup.memberRef(symLookup.stateGetter)), definitions.ObjectTpe) :: Nil)
      )
    }
    else callSetter
  }

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, adaptToUnit(rhs))

  // We use the convention that the state machine's ID for a state corresponding to
  // a labeldef will a negative number be based on the symbol ID. This allows us
  // to translate a forward jump to the label as a state transition to a known state
  // ID, even though the state machine transform hasn't yet processed the target label
  // def. Negative numbers are used so as as not to clash with regular state IDs, which
  // are allocated in ascending order from 0.
  private def stateIdForLabel(sym: Symbol): Int = -sym.id

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  // TODO AM: should this explode blocks even when expr is not ()?
  private def toList(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) if isLiteralUnit(expr) => stats
    case _ => tree :: Nil
  }

  private class replaceJumpsWithStateTransitions(states: StateSet) extends TypingTransformer(currentTransformState.unit) {
    object Thicket
    override def transform(tree: Tree): Tree = tree match {
      case Apply(fun, args) if isLabel(fun.symbol) =>
        labelDefStates.get(fun.symbol) match {
          case Some(i) =>
            states += i
            localTyper.typed(Block(mkStateTree(labelDefStates(fun.symbol)) :: Nil, Apply(Ident(currentTransformState.symLookup.whileLabel), Nil)).updateAttachment(Thicket))
          case None =>
            super.transform(tree)
        }
      case Block(stats, expr) =>
        val stats1 = mutable.ListBuffer[Tree]()
        transformTrees(stats).foreach {
          case blk @ Block(stats, expr) if blk.hasAttachment[Thicket.type] =>
            stats1 ++= stats
            stats1 += expr
          case t =>
            stats1 += t
        }
        val expr1 = transform(expr) match {
          case blk @ Block(stats, expr) if blk.hasAttachment[Thicket.type] =>
            stats1 ++= stats
            expr
          case expr =>
            expr
        }
        treeCopy.Block(tree, stats1.toList, expr1)
      case _ =>
        super.transform(tree)
    }
  }

}
