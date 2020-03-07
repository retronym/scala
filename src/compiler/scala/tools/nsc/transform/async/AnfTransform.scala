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

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.internal.Flags

private[async] trait AnfTransform extends TransformUtils {
  import global._

  /**
   * Transform `tree` into "A-Normal Form", such that within subtrees that enclose an `await`:
   *
   *   - `if`, `match`, and other control-flow constructs are only used as statements; they cannot be used as expressions;
   *   - calls to `await` are not allowed in compound expressions;
   *   - execution order is reified in the tree by extracting temporary vals
   */
  final def anfTransform(tree: Tree, owner: Symbol): Block = {
    val trans = new AnfTransformer()
    trans.atOwner(tree, owner) { trans.apply(tree) }
  }

  private final class AnfTransformer() extends TypingTransformer(currentTransformState.unit) {
    /** Main entry point to the ANF transform. */
    def apply(tree: Tree): Block = {
      transformNewControlFlowBlock(tree) match {
        case blk: Block => blk
        case t => atPos(t.pos)(Block(Nil, t).setType(t.tpe))
      }
    }

    // This transform typically transforms a single tree into a list of trees. This is somewhat awkward to
    // express as the standard `Transformer` doesn't support the notion of `Thickets` (a tree representing
    // as list of trees that will be flattened into its enclosing tree).
    //
    // Instead, `AnfTransformer` uses this mutable side-channel for the statements of the
    // current control flow block. This is convenient but requires some discipline: we need to
    // make sure we perform recursive transforms in the correct order (e.g. transform the
    // `qual` before the `args` of a `Apply`). This is the default transform behaviour and the
    // conventional way to write transforms in any case.
    private var currentStats = ListBuffer[Tree]()

    override def transform(tree: Tree): Tree = trace(tree) {
      curTree = tree
      val treeContainsAwait = containsAwait(tree)
      tree match {
        case _: ClassDef | _: ModuleDef | _: Function | _: DefDef =>
          tree
        case _: RefTree if tree.symbol.hasPackageFlag =>
          tree
        case _ if !treeContainsAwait =>
          tree
        case Apply(fun, args) if !Boolean_ShortCircuits.contains(fun.symbol) =>
          val lastAwaitArgIndex: Int = args.lastIndexWhere(containsAwait)
          val simpleFun = transform(fun)
          var i = 0
          val argExprss = map2(args, fun.symbol.paramss.head) { (arg: Tree, param: Symbol) =>
            transform(arg) match {
              case expr1 =>
                val argName = param.name.toTermName
                // No need to extract the argument into a val if is non-side-effecting or if we are beyond the final
                // argument containing an `await` calls.
                val elideVal = treeInfo.isExprSafeToInline(expr1) || lastAwaitArgIndex < 0 || i > lastAwaitArgIndex || !treeContainsAwait
                val result = if (elideVal) {
                  expr1
                } else {
                  if (isUnitType(expr1.tpe)) {
                    currentStats += expr1
                    literalBoxedUnit
                  } else {
                    val valDef = defineVal(name.freshen(argName), expr1, expr1.pos)
                    currentStats += valDef
                    gen.mkAttributedIdent(valDef.symbol)
                  }
                }
                i += 1
                result
            }
          }
          val simpleApply = treeCopy.Apply(tree, simpleFun, argExprss)
          simpleApply.attachments.remove[ContainsAwait.type]
          val isAwait = currentTransformState.ops.isAwait(fun)
          if (isAwait) {
            val valDef = defineVal(name.await(), treeCopy.Apply(tree, fun, argExprss), tree.pos)
            val ref = gen.mkAttributedStableRef(valDef.symbol).setType(tree.tpe)
            currentStats += valDef
            atPos(tree.pos)(ref)
          } else {
            simpleApply
          }

        case Block(stats, expr) =>
          // First, transform the block contents into a separate List[Tree]
          val (trees, _) = withNewControlFlowBlock {
            stats.foreach(stat => {
              val expr = transform(stat);
              if (!isLiteralUnit(expr)) currentStats += expr
            })
            currentStats += transform(expr)
            ()
          }

          // Identify groups of statements compiled from pattern matches and process them separately to
          // replace the label parameter of the `matchEnd` `LabelDef` with a `var matchRes: T` result var.
          //
          // The results are appended into the ambient `currentStats`, which has the desired effect of flattening
          // nested blocks.
          foreachGroupsEndingWith(trees)(
            isGroupEnd = isMatchEnd,
            onGroup = (ts: Array[Tree]) =>
              eliminateMatchEndLabelParameter(tree.pos, ts).foreach(t => flattenBlock(t)(currentStats += _)),
            onTail = (ts: List[Tree]) =>
              ts.foreach(t => flattenBlock(t)(currentStats += _))
          )

          // However, we let `onTail` add the expr to `currentStats` (that was more efficient than using `ts.dropRight(1).foreach(addToStats)`)
          // Compensate by removing it from the buffer and returning the expr.
          currentStats.remove(currentStats.size - 1)

        case ValDef(mods, name, tpt, rhs) => atOwner(tree.symbol) {
          // Capture current cursor of a non-empty `stats` buffer so we can efficiently restrict the
          // `changeOwner` to the newly added items...
          var statsIterator = if (currentStats.isEmpty) null else currentStats.iterator

          val expr = atOwner(currentOwner.owner)(transform(rhs))

          // But, ListBuffer.empty.iterator doesn't reflect later mutation. Luckily we can just start
          // from the beginning of the buffer
          if (statsIterator == null) statsIterator = currentStats.iterator

          // Definitions within stats lifted out of the `ValDef` rhs should no longer be owned by the
          // the ValDef.
          statsIterator.foreach(_.changeOwner((currentOwner, currentOwner.owner)))

          treeCopy.ValDef(tree, mods, name, tpt, expr)
        }

        case If(cond, thenp, elsep) =>
          val needsResultVar = (containsAwait(thenp) || containsAwait(elsep))
          transformMatchOrIf(tree, needsResultVar, name.ifRes) { varSym =>
            val condExpr = transform(cond)
            val thenBlock = transformNewControlFlowBlock(thenp)
            val elseBlock = transformNewControlFlowBlock(elsep)
            treeCopy.If(tree, condExpr, pushAssignmentIntoExpr(varSym, thenBlock), pushAssignmentIntoExpr(varSym, elseBlock))
          }

        case Match(scrut, cases) =>
          val needResultVar = cases.exists(containsAwait)
          transformMatchOrIf(tree, needResultVar, name.matchRes) { varSym =>
            val scrutExpr = transform(scrut)
            val casesWithAssign = cases map {
              case cd@CaseDef(pat, guard, body) =>
                assignUnitType(treeCopy.CaseDef(cd, pat, transformNewControlFlowBlock(guard), pushAssignmentIntoExpr(varSym, transformNewControlFlowBlock(body))))
            }
            treeCopy.Match(tree, scrutExpr, casesWithAssign)
          }

        case ld @ LabelDef(name, params, rhs) =>
          val rhs1 = transformNewControlFlowBlock(rhs)
          if (isCaseLabel(ld.symbol)) {
            object caseDefTransformer extends TypingTransformer(currentTransformState.unit) {
              var caseVars = ListBuffer[Tree]()
              var matchEndBlock: Block = null;
              override def transform(tree: Tree): Tree = tree match {
                case blk @ Block(stats, Apply(fun, _)) if isMatchEndLabel(fun.symbol) =>
                  matchEndBlock = blk
                  literalUnit
                case Block(stats, expr) =>
                  val stats1 = stats.mapConserve {
                    case vd: ValDef if vd.symbol.isSynthetic =>
                      vd.symbol.setFlag(Flags.MUTABLE)
                      caseVars += treeCopy.ValDef(vd, vd.mods, vd.name, vd.tpt, gen.mkZero(vd.symbol.info))
                      typedAssign(vd.rhs, vd.symbol)
                    case t =>
                      super.transform(t)
                  }
                  treeCopy.Block(tree, stats1, super.transform(expr))
                case _: LabelDef | _: MemberDef | _: Function =>
                  tree
                case _ =>
                  super.transform(tree)
              }
              def apply(tree: Tree): Tree = {
                caseVars += transform(tree)
                caseVars ++= matchEndBlock.stats
                if (matchEndBlock != null) {
                  treeCopy.Block(tree, caseVars.toList, matchEndBlock.expr)
                } else
                  tree
              }
            }
            val result = treeCopy.LabelDef(tree, name, params, caseDefTransformer.atOwner(tree, tree.symbol)(caseDefTransformer.apply(rhs1)))
            result
          } else {
            treeCopy.LabelDef(tree, name, params, rhs1)
          }

        case _ =>
          super.transform(tree)
      }
    }

    private def pushAssignmentIntoExpr(varSym: Symbol, t: Tree): Tree = {
      t match {
        case _ if varSym == NoSymbol || t.tpe.typeSymbol == definitions.NothingClass => t
        case MatchEnd(ld) => treeCopy.LabelDef(ld, ld.name, ld.params, pushAssignmentIntoExpr(varSym, ld.rhs))
        case b@Block(caseStats, caseExpr) => assignUnitType(treeCopy.Block(b, caseStats, pushAssignmentIntoExpr(varSym, caseExpr)))
        case _ => typedAssign(t, varSym)
      }
    }

    @tailrec
    private def transformMatchOrIf[T <: Tree](tree: Tree, needsResultVar: Boolean, nameSource: asyncNames.NameSource[TermName])(core: Symbol => T): Tree = {
      // if type of if/match is Unit don't introduce assignment,
      // but add Unit value to bring it into form expected by async transform
      if (typeEqualsUnit(tree.tpe)) {
        currentStats += assignUnitType(core(NoSymbol))
        atPos(tree.pos)(literalUnit)
      } else if (tree.tpe =:= definitions.NothingTpe) {
        currentStats += assignUnitType(core(NoSymbol))
        localTyper.typedPos(tree.pos)(Throw(New(IllegalStateExceptionClass)))
      } else if (isPatMatGeneratedJump(tree)) {
        transformMatchOrIf(assignUnitType(tree), needsResultVar, nameSource)(core)
      } else if (!needsResultVar) {
        core(NoSymbol)
      } else {
        val varDef = defineVar(nameSource(), tree.tpe, tree.pos)
        currentStats += varDef
        currentStats += assignUnitType(core(varDef.symbol))
        atPos(tree.pos)(gen.mkAttributedStableRef(varDef.symbol)).setType(tree.tpe)
      }
    }

    // Transform `tree` into with a new block. A new `currentStats` buffer will be pushed onto the stack and
    // the resulting stats will be included in the returned `Tree`. Use when the `tree` is not sequentially evaluated
    // after the preceding sibling, but rather will be the target of a control flow jump.
    private def transformNewControlFlowBlock(tree: Tree): Tree = {
      val savedStats = currentStats
      this.currentStats = new ListBuffer[Tree]
      try transform(tree) match {
        case b@Block(stats, expr) =>
          treeCopy.Block(b, currentStats.prependToList(stats), expr)
        case expr => currentStats.toList match {
          case Nil => expr
          case stats => treeCopy.Block(expr, stats, expr)
        }
      } finally {
        this.currentStats = savedStats
      }
    }

    private def withNewControlFlowBlock[T](f: => T): (List[Tree], T) = {
      val savedStats = currentStats
      this.currentStats = new ListBuffer[Tree]
      try {
        val result = f
        (currentStats.toList, result)
      } finally {
        this.currentStats = savedStats
      }
    }

    // If we run the ANF transform post patmat, deal with trees like `(if (cond) jump1(){String} else jump2(){String}){String}`
    // as though it was typed with `Unit`.
    private def isPatMatGeneratedJump(t: Tree): Boolean = t match {
      case Block(_, expr) => isPatMatGeneratedJump(expr)
      case If(_, thenp, elsep) => isPatMatGeneratedJump(thenp) && isPatMatGeneratedJump(elsep)
      case _: Apply if isLabel(t.symbol) => true
      case _ => false
    }

    /**
     * Identifies groups in a list of elements by a predicate on the terminal element.
     *
     * @param ts          The elements to be grouped
     * @param isGroupEnd  Identifies the terminal element of a group
     * @param onGroup     Callback to process each group
     * @param onTail      Callback to process the tail of the list that does not satisfy `isGroupEnd`
     */
    @tailrec
    private def foreachGroupsEndingWith[T <: AnyRef : reflect.ClassTag](ts: List[T])(isGroupEnd: T => Boolean, onGroup: Array[T] => Unit, onTail: List[T] => Unit): Unit = if (!ts.isEmpty) {
      ts.indexWhere(isGroupEnd) match {
        case -1 =>
          onTail(ts)
        case i =>
          val group = new Array[T](i + 1)
          ts.copyToArray(group)
          onGroup(group)
          foreachGroupsEndingWith(ts.drop(i + 1))(isGroupEnd, onGroup, onTail)
      }
    }

    // Replace the label parameters on `matchEnd` with use of a `matchRes` temporary variable
    //
    // CaseDefs are translated to labels without parameters. A terminal label, `matchEnd`, accepts
    // a parameter which is the result of the match (this is regular, so even Unit-typed matches have this).
    //
    // For our purposes, it is easier to:
    //   - extract a `matchRes` variable
    //   - rewrite the terminal label def to take no parameters, and instead read this temp variable
    //   - change jumps to the terminal label to an assignment and a no-arg label application
    def eliminateMatchEndLabelParameter(pos: Position, statsExpr: Array[Tree]): Iterator[Tree] = {
      val caseDefToMatchResult = collection.mutable.Map[Symbol, Symbol]()

      val matchResults = collection.mutable.Buffer[Tree]()

      def modifyLabelDef(ld: LabelDef): (Tree, Tree) = {
        val param = ld.params.head

        def unitLabelDef = {
          setUnitMethodInfo(ld.symbol)
          assignUnitType(treeCopy.LabelDef(ld, ld.name, Nil, literalUnit))
        }

        if (isUnitType(ld.params.head.tpe)) {
          // Unit typed match: eliminate the label def parameter, but don't create a matchres temp variable to
          // store the result for cleaner generated code.
          caseDefToMatchResult(ld.symbol) = NoSymbol
          (unitLabelDef, substituteTrees(ld.rhs, param.symbol :: Nil, literalUnit :: Nil))
        } else {
          // Otherwise, create the matchres var. We'll callers of the label def below.
          // Remember: we're iterating through the statement sequence in reverse, so we'll get
          // to the LabelDef and mutate `matchResults` before we'll get to its callers.
          val matchResult = defineVar(name.matchRes(), param.tpe, ld.pos)
          matchResults += matchResult
          caseDefToMatchResult(ld.symbol) = matchResult.symbol
          (unitLabelDef, ld.rhs.substituteSymbols(param.symbol :: Nil, matchResult.symbol :: Nil))
        }
      }

      val statsExpr0: ArrayBuffer[Tree] = new ArrayBuffer[Tree](statsExpr.length)

      statsExpr.reverseIterator.foreach {
        case ld@LabelDef(_, param :: Nil, _) =>
          val (ld1, after) = modifyLabelDef(ld)
          statsExpr0 += after
          statsExpr0 += ld1
        case a@ValDef(mods, name, tpt, ld@LabelDef(_, param :: Nil, _)) =>
          val (ld1, after) = modifyLabelDef(ld)
          statsExpr0 += treeCopy.ValDef(a, mods, name, tpt, after)
          statsExpr0 += ld1
        case t =>
          if (caseDefToMatchResult.isEmpty) statsExpr0 += t
          else {
            val matchResultTransformer = new MatchResultTransformer(caseDefToMatchResult)
            val tree1 = matchResultTransformer.transformAtOwner(currentOwner, t)
            statsExpr0 += tree1
          }
      }

      matchResults.toList match {
        case _ if caseDefToMatchResult.isEmpty =>
          statsExpr.iterator // return the original trees if nothing changed
        case Nil =>
          statsExpr0.reverseIterator ++ List(literalUnit) // must have been a unit-typed match, no matchRes variable to definne or refer to
        case r1 :: Nil =>
          // { var matchRes = _; ....; matchRes }
          List(r1).iterator ++ statsExpr0.reverseIterator
        case _ => global.reporter.error(pos, "Internal error: unexpected tree encountered during ANF transform " + statsExpr); statsExpr.iterator
      }
    }

    private final val traceAsync = false

    @inline final def trace[T](args: Any)(t: => T): T = {
      if (traceAsync) {
        tracing.apply("", args)({val tree = t; ("" + currentStats.mkString(";") + " ;; " + tree, tree)})
      } else t
    }

    def defineVal(name: global.TermName, rhs: global.Tree, pos: Position): ValDef = {
      val sym = currentOwner.newTermSymbol(name, pos, Flags.SYNTHETIC).setInfo(rhs.tpe)
      ValDef(sym, rhs.changeOwner((currentOwner, sym))).setType(NoType)
    }

    def defineVar(name: TermName, tp: Type, pos: Position): ValDef = {
      val sym = currentOwner.newTermSymbol(name, pos, Flags.MUTABLE | Flags.SYNTHETIC).setInfo(tp)
      ValDef(sym, gen.mkZero(tp).setPos(pos)).setType(NoType)
    }
  }

  private def typedAssign(lhs: Tree, varSym: Symbol) =
    Assign(gen.mkAttributedRef(varSym), lhs).setType(definitions.UnitTpe).setPos(lhs.pos)

  val tracing: Tracing
  class Tracing {
    private var indent = -1

    private def indentString = "  " * indent

    def apply[T](prefix: String, args: Any)(t: => (String, T)): T = {

      indent += 1

      def oneLine(s: Any) = s.toString.replaceAll("""\n""", "\\\\n").take(300)

      try {
        println(s"$indentString$prefix(${oneLine(args)})")
        val result = t
        println(s"$indentString= ${oneLine(result._1)}")
        result._2
      } finally {
        indent -= 1
      }
    }
  }

  final class MatchResultTransformer(caseDefToMatchResult: collection.Map[Symbol, Symbol]) extends TypingTransformer(currentTransformState.unit) {
    override def transform(tree: Tree): Tree = {
      tree match {
        case _: Function | _: MemberDef =>
          tree
        case Apply(fun, arg :: Nil) if isLabel(fun.symbol) && caseDefToMatchResult.contains(fun.symbol) =>
          val temp = caseDefToMatchResult(fun.symbol)
          if (temp == NoSymbol)
            treeCopy.Block(tree, transform(arg) :: Nil, treeCopy.Apply(tree, fun, Nil))
          else if (arg.tpe.typeSymbol == definitions.NothingClass) {
            transform(arg)
          } else {
            treeCopy.Block(tree, typedAssign(transform(arg), temp) :: Nil, treeCopy.Apply(tree, fun, Nil))
          }
        case Block(stats, expr: Apply) if isLabel(expr.symbol) =>
          super.transform(tree) match {
            case Block(stats0, Block(stats1, expr1)) =>
              // flatten the block returned by `case Apply` above into the enclosing block for
              // cleaner generated code.
              treeCopy.Block(tree, stats0 ::: stats1, expr1)
            case t => t
          }
        case _ =>
          super.transform(tree)
      }
    }
  }
}

object SyntheticBindVal
