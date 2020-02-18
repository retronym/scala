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

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.internal.Flags

private[async] trait AnfTransform extends TransformUtils {
  import global._
  final def anfTransform(tree: Tree, owner: Symbol): Block = {
    val trans = new AnfTransform(owner)
    // Must prepend the () for issue #31.
    val block = typecheck(atPos(tree.pos)(Block(List(literalUnit), tree))).setType(tree.tpe)
    val tree1 = adjustTypeOfTranslatedPatternMatches(block, owner)
    trans.transformAtOwner(owner, tree1).asInstanceOf[Block]
  }

  class AnfTransform(owner: Symbol) extends TypingTransformer(currentTransformState.unit) {

    sealed abstract class AnfMode

    case object Anf extends AnfMode

    case object Linearizing extends AnfMode

    var mode: AnfMode = Anf

    object trace {
      private var indent = -1

      private def indentString = "  " * indent

      def apply[T](args: Any)(t: => T): T = {
        def prefix = mode.toString.toLowerCase

        indent += 1

        def oneLine(s: Any) = s.toString.replaceAll("""\n""", "\\\\n").take(127)

        try {
          if (AsyncUtils.trace)
            AsyncUtils.trace(s"$indentString$prefix(${oneLine(args)})")
          val result = t
          if (AsyncUtils.trace)
            AsyncUtils.trace(s"$indentString= ${oneLine(result)}")
          result
        } finally {
          indent -= 1
        }
      }
    }

    def typed(tree: Tree) = localTyper.typed(tree)

    def typedAt(exprPos: Position, tree: Tree) = localTyper.typed(atPos(exprPos)(tree))

    def typedAssign(lhs: Tree, varSym: Symbol) =
      typedAt(lhs.pos, Assign(Ident(varSym), lhs))

    object linearize {
      def transformToList(tree: Tree): List[Tree] = {
        mode = Linearizing;
        blockToList(transform(tree))
      }
      def transformToStatsExpr(tree: Tree, stats: ListBuffer[Tree]): Tree = {
        mode = Linearizing;
        transform(tree) match {
          case blk: Block => stats ++= blk.stats; blk.expr
          case expr => expr
        }
      }

      def transformToBlock(tree: Tree): Block = {
        mode = Linearizing;
        transform(tree) match {
          case blk: Block =>
            blk
          case tree =>
            Block(Nil, tree).setPos(tree.pos).setType(tree.tpe)
        }
      }

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        val stats :+ expr = _anf.transformToList(tree)

        def statsExprUnit = {
          stats :+ expr :+ typedAt(expr.pos, literalUnit)
        }

        def statsExprThrow =
          stats :+ expr :+ typedAt(expr.pos, Throw(Apply(Select(New(gen.mkAttributedRef(IllegalStateExceptionClass)), nme.CONSTRUCTOR), Nil)))

        expr match {
          case Apply(fun, _) if currentTransformState.ops.isAwait(fun) =>
            val awaitResType = transformType(expr.tpe)
            val valDef = defineVal(name.await(), expr, tree.pos)(awaitResType)
            val ref = gen.mkAttributedStableRef(valDef.symbol).setType(awaitResType)
            stats :+ valDef :+ atPos(tree.pos)(ref)

          case If(cond, thenp, elsep) =>
            // If we run the ANF transform post patmat, deal with trees like `(if (cond) jump1(){String} else jump2(){String}){String}`
            // as though it was typed with `Unit`.
            def isPatMatGeneratedJump(t: Tree): Boolean = t match {
              case Block(_, expr) => isPatMatGeneratedJump(expr)
              case If(_, thenp, elsep) => isPatMatGeneratedJump(thenp) && isPatMatGeneratedJump(elsep)
              case _: Apply if isLabel(t.symbol) => true
              case _ => false
            }

            if (isPatMatGeneratedJump(expr))
              assignUnitType(expr)

            // if type of if-else is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (typeEqualsUnit(expr.tpe)) {
              statsExprUnit
            } else if (typeEqualsNothing(expr.tpe)) {
              statsExprThrow
            } else {
              val varDef = defineVar(name.ifRes(), expr.tpe, tree.pos)

              def branchWithAssign(t: Tree): Tree = {
                t match {
                  case MatchEnd(ld) =>
                    deriveLabelDef(ld, branchWithAssign)
                  case blk@Block(thenStats, thenExpr) =>
                    assignUnitType(treeCopy.Block(blk, thenStats, branchWithAssign(thenExpr)))
                  case _ =>
                    typedAssign(t, varDef.symbol)
                }
              }

              val ifWithAssign = assignUnitType(treeCopy.If(tree, cond, branchWithAssign(thenp), branchWithAssign(elsep)))
              stats :+ varDef :+ ifWithAssign :+ atPos(tree.pos)(gen.mkAttributedStableRef(varDef.symbol)).setType(tree.tpe)
            }
          case ld@LabelDef(name, params, rhs) =>
            if (isUnitType(ld.symbol.info.resultType)) statsExprUnit
            else stats :+ expr

          case Match(scrut, cases) =>
            // if type of match is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (typeEqualsUnit(expr.tpe)) {
              statsExprUnit
            } else if (typeEqualsNothing(expr.tpe)) {
              statsExprThrow
            } else {
              val varDef = defineVar(name.matchRes(), expr.tpe, tree.pos)
              val casesWithAssign = cases map {
                case cd@CaseDef(pat, guard, body) =>
                  def bodyWithAssign(t: Tree): Tree = {
                    t match {
                      case MatchEnd(ld) => deriveLabelDef(ld, bodyWithAssign)
                      case b@Block(caseStats, caseExpr) => assignUnitType(treeCopy.Block(b, caseStats, bodyWithAssign(caseExpr)))
                      case _ => typedAssign(t, varDef.symbol)
                    }
                  }

                  assignUnitType(treeCopy.CaseDef(cd, pat, guard, bodyWithAssign(body)))
              }
              val matchWithAssign = assignUnitType(treeCopy.Match(tree, scrut, casesWithAssign))
              require(matchWithAssign.tpe != null, matchWithAssign)
              stats :+ varDef :+ matchWithAssign :+ atPos(tree.pos)(gen.mkAttributedStableRef(varDef.symbol)).setType(tree.tpe)
            }
          case _ =>
            stats :+ expr
        }
      }

      def defineVar(name: TermName, tp: Type, pos: Position): ValDef = {
        val sym = currentOwner.newTermSymbol(name, pos, Flags.MUTABLE | Flags.SYNTHETIC).setInfo(transformType(tp))
        ValDef(sym, mkZero(tp, pos)).setType(NoType).setPos(pos)
      }
    }

    def defineVal(name: TermName, lhs: Tree, pos: Position)(tp: Type = transformType(lhs.tpe)): ValDef = {
      val sym = currentOwner.newTermSymbol(name, pos, Flags.SYNTHETIC).setInfo(tp)

      val lhsOwned = lhs.changeOwner((currentOwner, sym))
      val rhs =
        if (isPastErasure && isUnitType(tp)) Block(lhsOwned :: Nil, literalUnit)
        else lhsOwned
      ValDef(sym, rhs).setType(NoType).setPos(pos)

    }

    object _anf {
      import treeInfo.Applied

      def transformToList(tree: Tree): List[Tree] = {
        mode = Anf;
        blockToList(transform(tree))
      }

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        val trees = new ListBuffer[Tree]
        _transformToList(tree, trees)
        trees.toList
      }

      def _transformToList(tree: Tree, stats: ListBuffer[Tree]): Unit = {
        if (!containsAwait(tree)) {
          tree match {
            case blk : Block =>
              // avoids nested block in `while(await(false)) ...`.
              // TODO I think `containsAwait` really should return true if the code contains a label jump to an enclosing
              // while/doWhile and there is an await *anywhere* inside that construct.
              stats ++= blk.stats
              stats += blk.expr
            case _ =>
              stats += tree
          }
        } else tree match {
          case Select(qual, sel) =>
            val expr = linearize.transformToStatsExpr(qual, stats)
            stats += treeCopy.Select(tree, expr, sel)

          case Throw(expr) =>
            val expr1 = linearize.transformToStatsExpr(expr, stats)
            stats += treeCopy.Throw(tree, expr1)

          case Typed(expr, tpt) =>
            val expr1 = linearize.transformToStatsExpr(expr, stats)
            stats += treeCopy.Typed(tree, expr1, tpt)

          case ArrayValue(elemtp, elems) =>
            val elemExprs = elems.mapConserve(elem => linearize.transformToStatsExpr(elem, stats))
            stats += treeCopy.ArrayValue(tree, elemtp, elemExprs)

          case Applied(fun, targs, argss) if argss.nonEmpty =>
            // we can assume that no await call appears in a by-name argument position,
            // this has already been checked.
            val simpleFun = linearize.transformToStatsExpr(fun, stats)
            val argExprss: List[List[Tree]] =
              mapArgumentss(fun, argss) {
                case Arg(expr, byName, _) if byName /*|| isPure(expr) TODO */ => expr
                case Arg(expr, _, argName) =>
                  linearize.transformToStatsExpr(expr, stats) match {
                    case expr1 =>
                      val valDef = defineVal(name.freshen(argName), expr1, expr1.pos)()
                      require(valDef.tpe != null, valDef)
                      stats += valDef
                      atPos(tree.pos.makeTransparent)(gen.stabilize(gen.mkAttributedIdent(valDef.symbol)))
                  }
              }

            def copyApplied(tree: Tree, depth: Int): Tree = {
              tree match {
                case TypeApply(_, targs) => treeCopy.TypeApply(tree, simpleFun, targs)
                case _ if depth == 0 => simpleFun
                case Apply(fun, args) =>
                  val newTypedArgs = map2(args.map(_.pos), argExprss(depth - 1))((pos, arg) => typedAt(pos, arg))
                  treeCopy.Apply(tree, copyApplied(fun, depth - 1), newTypedArgs)
              }
            }

            val typedNewApply = copyApplied(tree, argss.length)

            stats += typedNewApply

          case blk: Block =>
            val stats1 = blk.stats.flatMap(linearize.transformToList).filterNot(isLiteralUnit)
            val exprs1 = linearize.transformToList(blk.expr)
            val trees = stats1 ::: exprs1

            def foreachGroupsEndingWith(ts: List[Tree])(isGroupEnd: Tree => Boolean, onGroup: Array[Tree] => Unit, onTail: List[Tree] => Unit): Unit = if (ts.isEmpty) () else {
              ts.indexWhere(isGroupEnd) match {
                case -1 => onTail(ts)
                case i =>
                  val group = new Array[Tree](i + 1)
                  ts.copyToArray(group)
                  onGroup(group)
                  foreachGroupsEndingWith(ts.drop(i + 1))(isGroupEnd, onGroup, onTail)
              }
            }
            def addToStats(t: Tree): Unit = t match {
              case blk1: Block =>
                stats ++= blk1.stats
                stats += blk1.expr
              case t =>
                stats += t
            }

            foreachGroupsEndingWith(trees)(
              isGroupEnd = ({ case MatchEnd(_) => true; case _ => false }),
              onGroup = (ts: Array[Tree]) => eliminateMatchEndLabelParameter(tree.pos, ts).foreach(addToStats),
              onTail = (ts: List[Tree]) => ts.foreach(addToStats)
            )

          case ValDef(mods, name, tpt, rhs) =>
            if (containsAwait(rhs)) {
              // Capture current cursor of a non-empty `stats` buffer so we can efficiently restrict the
              // `changeOwner` to the newly added items...
              var statsIterator = if (stats.isEmpty) null else stats.iterator

              val expr = atOwner(currentOwner.owner)(linearize.transformToStatsExpr(rhs, stats))

              // But, ListBuffer.empty.iterator doesn't reflect later mutation. Luckily we can just start
              // from the beginning of the buffer
              if (statsIterator == null) statsIterator = stats.iterator
              statsIterator.foreach(_.changeOwner((currentOwner, currentOwner.owner)))

              stats += treeCopy.ValDef(tree, mods, name, tpt, expr)
            } else {
              stats += tree
            }

          case Assign(lhs, rhs) =>
            val expr = linearize.transformToStatsExpr(rhs, stats)
            stats += treeCopy.Assign(tree, lhs, expr)

          case If(cond, thenp, elsep) =>
            val condExpr = linearize.transformToStatsExpr(cond, stats)
            val thenBlock = linearize.transformToBlock(thenp)
            val elseBlock = linearize.transformToBlock(elsep)
            stats += treeCopy.If(tree, condExpr, thenBlock, elseBlock)

          case Match(scrut, cases) =>
            val scrutExpr = linearize.transformToStatsExpr(scrut, stats)
            val caseDefs = cases mapConserve {
              case CaseDef(pat, guard, body) =>
                // extract local variables for all names bound in `pat`, and rewrite `body`
                // to refer to these.
                // TODO we can move this into ExprBuilder once we get rid of `AsyncDefinitionUseAnalyzer`.
                val block = linearize.transformToBlock(body)
                val newBlockStats = ListBuffer[Tree]()
                val from = ListBuffer[Symbol]()
                val to = ListBuffer[Symbol]()
                pat foreach {
                  case b@Bind(bindName, _) =>
                    val vd = defineVal(name.freshen(bindName.toTermName), gen.mkAttributedStableRef(b.symbol).setPos(b.pos), b.pos)()
                    vd.symbol.updateAttachment(SyntheticBindVal)
                    newBlockStats += vd
                    from += b.symbol
                    to += vd.symbol
                  case _ =>
                }
                val b@Block(stats1, expr1) = block.substituteSymbols(from.toList, to.toList).asInstanceOf[Block]
                newBlockStats ++= stats1
                val newBlock = treeCopy.Block(b, newBlockStats.toList, expr1)
                treeCopy.CaseDef(tree, pat, guard, newBlock)
            }
            stats += treeCopy.Match(tree, scrutExpr, caseDefs)

          case LabelDef(name, params, rhs) =>
            if (!isPastErasure && isUnitType(tree.symbol.info)) // erasure has already inserted unit
              stats += treeCopy.LabelDef(tree, name, params, typed(Block(linearize.transformToList(rhs), literalUnit))).setSymbol(tree.symbol)
            else
              stats += treeCopy.LabelDef(tree, name, params, typed(linearize.transformToBlock(rhs))).setSymbol(tree.symbol)

          case TypeApply(fun, targs) =>
            val simpleFun = linearize.transformToStatsExpr(fun, stats)
            stats += treeCopy.TypeApply(tree, simpleFun, targs)

          case _ =>
            stats += tree
        }
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
          assignUnitType(treeCopy.LabelDef(ld, ld.name, Nil, typed(literalUnit)))
        }

        if (isUnitType(ld.params.head.tpe)) {
          // Unit typed match: eliminate the label def parameter, but don't create a matchres temp variable to
          // store the result for cleaner generated code.
          caseDefToMatchResult(ld.symbol) = NoSymbol
          (unitLabelDef, substituteTrees(ld.rhs, param.symbol :: Nil, typed(literalUnit) :: Nil))
        } else {
          // Otherwise, create the matchres var. We'll callers of the label def below.
          // Remember: we're iterating through the statement sequence in reverse, so we'll get
          // to the LabelDef and mutate `matchResults` before we'll get to its callers.
          val matchResult = linearize.defineVar(name.matchRes(), param.tpe, ld.pos)
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
            val tree1 = matchResultTransformer.transformAtOwner(owner, t)
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
          List(r1).iterator ++ statsExpr0.reverseIterator ++ List(atPos(pos)(gen.mkAttributedIdent(r1.symbol)))
        case _ => error(pos, "Internal error: unexpected tree encountered during ANF transform " + statsExpr); statsExpr.iterator
      }
    }

    def anfLinearize(tree: Tree): Block = {
      val trees: List[Tree] = mode match {
        case Anf => _anf._transformToList(tree)
        case Linearizing => linearize._transformToList(tree)
      }
      listToBlock(trees)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case _: ValDef | _: DefDef | _: Function | _: ClassDef | _: TypeDef =>
          atOwner(tree.symbol)(anfLinearize(tree))
        case _: ModuleDef =>
          atOwner(tree.symbol.asModule.moduleClass orElse tree.symbol)(anfLinearize(tree))
        case _ =>
          anfLinearize(tree)
      }
    }
  }

  final class MatchResultTransformer(caseDefToMatchResult: collection.Map[Symbol, Symbol]) extends TypingTransformer(currentTransformState.unit) {
    override def transform(tree: Tree): Tree = {
      def typedPos(pos: Position)(t: Tree): Tree = localTyper.typed(atPos(pos)(t))

      tree match {
        case Apply(fun, arg :: Nil) if isLabel(fun.symbol) && caseDefToMatchResult.contains(fun.symbol) =>
          val temp = caseDefToMatchResult(fun.symbol)
          if (temp == NoSymbol)
            typedPos(tree.pos)(Block(transform(arg) :: Nil, treeCopy.Apply(tree, fun, Nil)))
          else
          // setType needed for LateExpansion.shadowingRefinedType test case. There seems to be an inconsistency
          // in the trees after pattern matcher.
          // TODO miminize the problem in patmat and fix in scalac.
            typedPos(tree.pos)(Block(Assign(Ident(temp), transform(arg.setType(transformType(fun.tpe.paramLists.head.head.info)))) :: Nil, treeCopy.Apply(tree, fun, Nil)))
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
