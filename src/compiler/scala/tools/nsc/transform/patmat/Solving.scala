/* NSC -- new Scala compiler
 *
 * Copyright 2011-2017 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import java.util

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable}
import scala.reflect.internal.util.Collections._
import scala.reflect.internal.util.{IntArraySet, Position, StatisticsStatics}

// a literal is a (possibly negated) variable
class Lit(val v: Int) extends AnyVal {
  def unary_- : Lit = Lit(-v)

  def variable: Int = Math.abs(v)

  def positive = v >= 0

  override def toString(): String = s"Lit#$v"
}

object Lit {
  def apply(v: Int): Lit = new Lit(v)

  implicit val LitOrdering: Ordering[Lit] = Ordering.by(_.v)
}

/** Solve pattern matcher exhaustivity problem via DPLL.
 */
trait Solving extends Logic {
  import global.statistics

  trait CNF extends PropositionalLogic {

    type Clause  = IntArraySet

    // a clause is a disjunction of distinct literals
    def clause(): Clause = new IntArraySet
    def clause(l: Lit): Clause = {
      val result = new IntArraySet
      result += l.v
      result
    }
    def clause(l: Lit, l2: Lit): Clause = {
      val result = new IntArraySet
      result += l.v
      result += l2.v
      result
    }
    def clause(l: Lit, l2: Lit, ls: Lit*): Clause = {
      val result = new IntArraySet
      result += l.v
      result += l2.v
      ls.foreach(l => result += l.v)
      result
    }

    /** Conjunctive normal form (of a Boolean formula).
     *  A formula in this form is amenable to a SAT solver
     *  (i.e., solver that decides satisfiability of a formula).
     */
    type Cnf = Array[Clause]

    class SymbolMapping(symbols: Set[Sym]) {
      val variableForSymbol: Map[Sym, Int] = {
        symbols.zipWithIndex.map {
          case (sym, i) => sym -> (i + 1)
        }.toMap
      }

      val symForVar: Map[Int, Sym] = variableForSymbol.map(_.swap)

      val relevantVars =
        symForVar.keysIterator().map(math.abs).to(immutable.BitSet)

      def lit(sym: Sym): Lit = Lit(variableForSymbol(sym))

      def size = symbols.size
    }

    def cnfString(f: Array[Clause]): String

    final case class Solvable(cnf: Cnf, symbolMapping: SymbolMapping) {
      def ++(other: Solvable) = {
        require(this.symbolMapping eq other.symbolMapping)
        Solvable(cnf ++ other.cnf, symbolMapping)
      }

      override def toString: String = {
        "Solvable\nLiterals:\n" +
          (for {
            (lit, sym) <- symbolMapping.symForVar.toSeq.sortBy(_._1)
          } yield {
            s"$lit -> $sym"
          }).mkString("\n") + "Cnf:\n" + cnfString(cnf)
      }
    }

    trait CnfBuilder {
      private[this] val buff = ArrayBuffer[Clause]()

      var literalCount: Int

      /**
       * @return new Tseitin variable
       */
      def newLiteral(): Lit = {
        literalCount += 1
        Lit(literalCount)
      }

      lazy val constTrue: Lit = {
        val constTrue = newLiteral()
        addClauseProcessed(clause(constTrue))
        constTrue
      }

      def constFalse: Lit = -constTrue

      def isConst(l: Lit): Boolean = l == constTrue || l == constFalse

      def addClauseProcessed(clause: Clause): Unit = {
        if (clause.nonEmpty) {
          buff += clause
        }
      }

      def buildCnf: Array[Clause] = {
        val cnf = buff.toArray
        buff.clear()
        cnf
      }

    }

    /** Plaisted transformation: used for conversion of a
      * propositional formula into conjunctive normal form (CNF)
      * (input format for SAT solver).
      * A simple conversion into CNF via Shannon expansion would
      * also be possible but it's worst-case complexity is exponential
      * (in the number of variables) and thus even simple problems
      * could become untractable.
      * The Plaisted transformation results in an _equisatisfiable_
      * CNF-formula (it generates auxiliary variables)
      * but runs with linear complexity.
      * The common known Tseitin transformation uses bi-implication,
      * whereas the Plaisted transformation uses implication only, thus
      * the resulting CNF formula has (on average) only half of the clauses
      * of a Tseitin transformation.
      * The Plaisted transformation uses the polarities of sub-expressions
      * to figure out which part of the bi-implication can be omitted.
      * However, if all sub-expressions have positive polarity
      * (e.g., after transformation into negation normal form)
      * then the conversion is rather simple and the pseudo-normalization
      * via NNF increases chances only one side of the bi-implication
      * is needed.
      */
    class TransformToCnf(symbolMapping: SymbolMapping) extends CnfBuilder {

      // new literals start after formula symbols
      var literalCount: Int = symbolMapping.size

      def convertSym(sym: Sym): Lit = symbolMapping.lit(sym)

      def apply(p: Prop): Solvable = {
        def mapProps(s: Set[Prop])(f: Prop => Option[Lit]): Clause = {
          val result = new IntArraySet
          for (prop <- s) {
            f(prop) match {
              case Some(lit) => result += lit.v
              case None =>
            }
          }
          result
        }

        def convert(p: Prop): Option[Lit] = {
          p match {
            case And(fv)  =>
              Some(and(mapProps(fv)(convert)))
            case Or(fv)   =>
              Some(or(mapProps(fv)(convert)))
            case Not(a)   =>
              convert(a).map(not)
            case sym: Sym =>
              Some(convertSym(sym))
            case True     =>
              Some(constTrue)
            case False    =>
              Some(constFalse)
            case AtMostOne(ops) =>
              atMostOne(ops)
              None
            case _: Eq    =>
              throw new MatchError(p)
          }
        }

        def and(bv: Clause): Lit = {
          if (bv.isEmpty) {
            // this case can actually happen because `removeVarEq` could add no constraints
            constTrue
          } else if (bv.size == 1) {
            Lit(bv.head)
          } else if (bv.contains(constFalse.v)) {
            constFalse
          } else {
            // op1 /\ op2 /\ ... /\ opx <==>
            // (o -> op1) /\ (o -> op2) ... (o -> opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            // (!o \/ op1) /\ (!o \/ op2) ... (!o \/ opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            bv -= constTrue.v // ignore `True`
            val o = newLiteral() // auxiliary Tseitin variable
            bv.foreach(op => addClauseProcessed(clause(Lit(op), -o)))
            o
          }
        }

        def or(bv: Clause): Lit = {
          if (bv.isEmpty) {
            constFalse
          } else if (bv.size == 1) {
            Lit(bv.head)
          } else if (bv.contains(constTrue.v)) {
            constTrue
          } else {
            // op1 \/ op2 \/ ... \/ opx <==>
            // (op1 -> o) /\ (op2 -> o) ... (opx -> o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            // (!op1 \/ o) /\ (!op2 \/ o) ... (!opx \/ o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            val new_bv = bv.clone() // TODO can we skip this?
            new_bv -= constFalse.v // ignore `False`
            val o = newLiteral() // auxiliary Tseitin variable
            new_bv += (-o.v)
            addClauseProcessed(new_bv)
            o
          }
        }

        // no need for auxiliary variable
        def not(a: Lit): Lit = -a

        /**
         * This encoding adds 3n-4 variables auxiliary variables
         * to encode that at most 1 symbol can be set.
         * See also "Towards an Optimal CNF Encoding of Boolean Cardinality Constraints"
         * http://www.carstensinz.de/papers/CP-2005.pdf
         */
        def atMostOne(ops: List[Sym]): Unit = {
          (ops: @unchecked) match {
            case hd :: Nil  => convertSym(hd)
            case x1 :: tail =>
              // sequential counter: 3n-4 clauses
              // pairwise encoding: n*(n-1)/2 clauses
              // thus pays off only if n > 5
              if (ops.lengthCompare(5) > 0) {

                @inline
                def /\(a: Lit, b: Lit) = addClauseProcessed(clause(a, b))

                val (mid, xn :: Nil) = tail.splitAt(tail.size - 1)

                // 1 <= x1,...,xn <==>
                //
                // (!x1 \/ s1) /\ (!xn \/ !sn-1) /\
                //
                //     /\
                //    /  \ (!xi \/ si) /\ (!si-1 \/ si) /\ (!xi \/ !si-1)
                //  1 < i < n
                val s1 = newLiteral()
                /\(-convertSym(x1), s1)
                val snMinus = mid.foldLeft(s1) {
                  case (siMinus, sym) =>
                    val xi = convertSym(sym)
                    val si = newLiteral()
                    /\(-xi, si)
                    /\(-siMinus, si)
                    /\(-xi, -siMinus)
                    si
                }
                /\(-convertSym(xn), -snMinus)
              } else {
                ops.map(convertSym).combinations(2).foreach {
                  case a :: b :: Nil =>
                    addClauseProcessed(clause(-a, -b))
                  case _             =>
                }
              }
          }
        }

        // add intermediate variable since we want the formula to be SAT!
        convert(p) match {
          case Some(lit) => addClauseProcessed(clause(lit))
          case None =>
        }

        Solvable(buildCnf, symbolMapping)
      }
    }

    class AlreadyInCNF(symbolMapping: SymbolMapping) {

      object ToLiteral {
        def unapply(f: Prop): Option[Lit] = f match {
          case Not(ToLiteral(lit)) => Some(-lit)
          case sym: Sym            => Some(symbolMapping.lit(sym))
          case _                   => None
        }
      }

      object ToDisjunction {
        def unapply(f: Prop): Option[Array[Clause]] = f match {
          case Or(fv)         =>
            val result = new IntArraySet
            var it = fv.iterator
            while (it.hasNext) {
              it.next() match {
                case ToLiteral(lit) => result += lit.v
                case _ => return None
              }
            }
            Some(Array(result))
          case True           => Some(Array()) // empty, no clauses needed
          case False          => Some(Array(clause())) // empty clause can't be satisfied
          case ToLiteral(lit) => Some(Array(clause(lit)))
          case _              => None
        }
      }

      /**
       * Checks if propositional formula is already in CNF
       */
      object ToCnf {
        def unapply(f: Prop): Option[Solvable] = f match {
          case ToDisjunction(clauses) => Some(Solvable(clauses, symbolMapping) )
          case And(fv)                =>
            val clauses = fv.foldLeft(Option(mutable.ArrayBuffer[Clause]())) {
              case (Some(cnf), ToDisjunction(clauses)) =>
                Some(cnf ++= clauses)
              case (_, _)                              =>
                None
            }
            clauses.map(c => Solvable(c.toArray, symbolMapping))
          case _                      => None
        }
      }
    }

    def eqFreePropToSolvable(p: Prop): Solvable = {

      def doesFormulaExceedSize(p: Prop): Boolean = {
        p match {
          case And(ops) =>
            if (ops.size > AnalysisBudget.maxFormulaSize) {
              true
            } else {
              ops.exists(doesFormulaExceedSize)
            }
          case Or(ops)  =>
            if (ops.size > AnalysisBudget.maxFormulaSize) {
              true
            } else {
              ops.exists(doesFormulaExceedSize)
            }
          case Not(a)   => doesFormulaExceedSize(a)
          case _        => false
        }
      }

      val simplified = simplify(p)
      if (doesFormulaExceedSize(simplified)) {
        throw AnalysisBudget.formulaSizeExceeded
      }

      // collect all variables since after simplification / CNF conversion
      // they could have been removed from the formula
      val symbolMapping = new SymbolMapping(gatherSymbols(p))
      val cnfExtractor = new AlreadyInCNF(symbolMapping)
      val cnfTransformer = new TransformToCnf(symbolMapping)

      def cnfFor(prop: Prop): Solvable = {
        prop match {
          case cnfExtractor.ToCnf(solvable) =>
            // this is needed because t6942 would generate too many clauses with Tseitin
            // already in CNF, just add clauses
            solvable
          case p                            =>
            cnfTransformer.apply(p)
        }
      }

      simplified match {
        case And(props) =>
          // scala/bug#6942:
          // CNF(P1 /\ ... /\ PN) == CNF(P1) ++ CNF(...) ++ CNF(PN)
          props.iterator.map(cnfFor).reduce(_ ++ _)
        case p          =>
          cnfFor(p)
      }
    }
  }

  // simple solver using DPLL
  trait Solver extends CNF {
    import scala.collection.mutable.ArrayBuffer

    def cnfString(f: Array[Clause]): String = {
      val lits: Array[List[String]] = f map (_.iterator.map(_.toString).toList)
      val xss: List[List[String]] = lits.toList
      val aligned: String = alignAcrossRows(xss, "\\/", " /\\\n")
      aligned
    }

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)

    // empty set of clauses is trivially satisfied
    val EmptyModel = Map.empty[Sym, Boolean]

    // no model: originates from the encounter of an empty clause, i.e.,
    // happens if all variables have been assigned in a way that makes the corresponding literals false
    // thus there is no possibility to satisfy that clause, so the whole formula is UNSAT
    val NoModel: Model = null

    // this model contains the auxiliary variables as well
    type TseitinModel = IntArraySet
    val EmptyTseitinModel = new IntArraySet
    val NoTseitinModel: TseitinModel = null

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(solvable: Solvable, pos: Position): List[Solution] = {
      debug.patmat("find all models for\n"+ cnfString(solvable.cnf))

      // we must take all vars from non simplified formula
      // otherwise if we get `T` as formula, we don't expand the variables
      // that are not in the formula...
      val relevantVars: immutable.BitSet = solvable.symbolMapping.relevantVars

      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      // (i.e. the blocking clause - used for ALL-SAT)
      def negateModel(m: TseitinModel) = {
        // filter out auxiliary Tseitin variables
        val result = new IntArraySet
        m.iterator().filter((l: Int) => relevantVars.contains(Lit(l).variable)).foreach((lit: Int) => result += -Lit(lit).v)
        result
      }

      final case class TseitinSolution(model: TseitinModel, unassigned: List[Int]) {
        def projectToSolution(symForVar: Map[Int, Sym]) = Solution(projectToModel(model, symForVar), unassigned map symForVar)
      }

      def findAllModels(clauses: Array[Clause],
                        models: List[TseitinSolution],
                        recursionDepthAllowed: Int = AnalysisBudget.maxDPLLdepth): List[TseitinSolution]=
        if (recursionDepthAllowed == 0) {
          uncheckedWarning(pos, AnalysisBudget.recursionDepthReached)
          models
        } else {
          debug.patmat("find all models for\n" + cnfString(clauses))
          val model = findTseitinModelFor(clauses)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoTseitinModel) {
            // note that we should not expand the auxiliary variables (from Tseitin transformation)
            // since they are existentially quantified in the final solution
            val unassigned: List[Int] = relevantVars.iterator().filterNot(i => model.contains(i) || model.contains(-i)).toList
            debug.patmat("unassigned "+ unassigned +" in "+ model)

            val solution = TseitinSolution(model, unassigned)
            val negated = negateModel(model)
            findAllModels(clauses :+ negated, solution :: models, recursionDepthAllowed - 1)
          }
          else models
        }

      val tseitinSolutions = findAllModels(solvable.cnf, Nil)
      tseitinSolutions.map(_.projectToSolution(solvable.symbolMapping.symForVar))
    }

    /** Drop trivially true clauses, simplify others by dropping negation of `unitLit`.
     *
     *  Disjunctions that contain the literal we're making true in the returned model are trivially true.
     *  Clauses can be simplified by dropping the negation of the literal we're making true
     *  (since False \/ X == X)
     */
    private def dropUnit(clauses: Array[Clause], unitLit: Lit): Unit = {
      val negated = -unitLit
      var i, j = 0
      while (i < clauses.length) {
        val clause = clauses(i)
        if (clause == null) return
        clauses(i) = null
        if (!clause.contains(unitLit.v)) {
          val simplifiedClause = if (clause.contains(negated.v)) {
            val clone = clause.clone()
            clone -= negated.v
            clone
          } else clause
          clauses(j) = simplifiedClause
          j += 1
        }
        i += 1
      }
    }

    def findModelFor(solvable: Solvable): Model = {
      projectToModel(findTseitinModelFor(solvable.cnf), solvable.symbolMapping.symForVar)
    }

    def findTseitinModelFor(clauses: Array[Clause]): TseitinModel = {
      debug.patmat(s"DPLL\n${cnfString(clauses)}")

      val start = if (StatisticsStatics.areSomeColdStatsEnabled) statistics.startTimer(statistics.patmatAnaDPLL) else null

      val satisfiableWithModel = findTseitinModel0((util.Arrays.copyOf(clauses, clauses.length), EmptyTseitinModel) :: Nil)

      if (StatisticsStatics.areSomeColdStatsEnabled) statistics.stopTimer(statistics.patmatAnaDPLL, start)
      satisfiableWithModel
    }

    type TseitinSearch = List[(Array[Clause], IntArraySet)]

    /** An implementation of the DPLL algorithm for checking statisfiability
      * of a Boolean formula in CNF (conjunctive normal form).
      *
      * This is a backtracking, depth-first algorithm, which searches a
      * (conceptual) decision tree the nodes of which represent assignments
      * of truth values to variables. The algorithm works like so:
      *
      * - If there are any empty clauses, the formula is unsatisifable.
      * - If there are no clauses, the formula is trivially satisfiable.
      * - If there is a clause with a single positive (rsp. negated) variable
      *   in it, any solution must assign it the value `true` (rsp. `false`).
      *   Therefore, assign it that value, and perform Boolean Constraint
      *   Propagation on the remaining clauses:
      *   - Any disjunction containing the variable in a positive (rsp. negative)
      *     usage is trivially true, and can be dropped.
      *   - Any disjunction containing the variable in a negative (rsp. positive)
      *     context will not be satisfied using that variable, so it can be
      *     removed from the disjunction.
      * - Otherwise, pick a variable:
      *   - If it always (rsp. never) appears negated (a pure variable), then
      *     any solution must assign the value `true` to it (rsp. `false`)
      *   - Otherwise, try to solve the formula assuming that the variable is
      *     `true`; if no model is found, try to solve assuming it is `false`.
      *
      * See also [[https://en.wikipedia.org/wiki/DPLL_algorithm]].
      *
      * This implementation uses a `List` to reify the seach stack, thus making
      * it run in constant stack space. The stack is composed of pairs of
      * `(remaining clauses, variable assignments)`, and depth-first search
      * is achieved by using a stack rather than a queue.
      *
      */
    @annotation.tailrec
    private def findTseitinModel0(state: TseitinSearch): TseitinModel = {
      state match {
        case Nil => NoTseitinModel
        case (clauses, assignments) :: rest =>
          if (clauses.isEmpty || clauses.head == null) assignments
          else if (clauses exists (clause => clause != null && clause.isEmpty)) findTseitinModel0(rest)
          else clauses.find(clause => clause != null && clause.size == 1) match {
            case Some(unitClause) =>
              val unitLit = Lit(unitClause.head)
              val newAssignments = assignments.clone()
              newAssignments += unitLit.v
              dropUnit(clauses, unitLit)
              val tuples: TseitinSearch = (clauses, newAssignments) :: rest
              findTseitinModel0(tuples)
            case _ =>
              // partition symbols according to whether they appear in positive and/or negative literals
              val pos = new mutable.BitSet()
              val neg = new mutable.BitSet()
              for (clause <- clauses) {
                if (clause != null) {
                  clause.foreach { i: Int =>
                    val lit = Lit(i)
                    if (lit.positive) pos += lit.variable else neg += lit.variable
                  }
                }
              }

              // appearing only in either positive/negative positions
              val pures = pos ^ neg

              if (pures.nonEmpty) {
                val pureVar = pures.head
                // turn it back into a literal
                // (since equality on literals is in terms of equality
                //  of the underlying symbol and its positivity, simply construct a new Lit)
                val pureLit = Lit(if (neg(pureVar)) -pureVar else pureVar)
                // debug.patmat("pure: "+ pureLit +" pures: "+ pures)
                val simplified = clauses.filterNot(clause => clause != null && clause.contains(pureLit.v))
                val newAssignments = assignments.clone()
                newAssignments += pureLit.v
                findTseitinModel0((simplified, newAssignments) :: rest)
              } else {
                val split = Lit(clauses.find(_ != null).get.head)
                // debug.patmat("split: "+ split)
                var i = 0
                var nullIndex = -1
                while (i < clauses.length && nullIndex == -1) {
                  if (clauses(i) eq null) nullIndex = i
                  i += 1
                }

                val effectiveLength = if (nullIndex == -1) clauses.length else nullIndex
                val posClauses = util.Arrays.copyOf(clauses, effectiveLength + 1)
                val negClauses = util.Arrays.copyOf(clauses, effectiveLength + 1)
                posClauses(effectiveLength) = clause(split)
                negClauses(effectiveLength) = clause(-split)

                val pos = (posClauses, assignments)
                val neg = (negClauses, assignments)
                findTseitinModel0(pos :: neg :: rest)
              }
          }
      }
    }

    private def projectToModel(model: TseitinModel, symForVar: Map[Int, Sym]): Model =
      if (model == NoTseitinModel) NoModel
      else if (model == EmptyTseitinModel) EmptyModel
      else {
        val mappedModels = model.iterator().toList collect {
          case lit if symForVar isDefinedAt Lit(lit).variable => (symForVar(Lit(lit).variable), Lit(lit).positive)
        }
        if (mappedModels.isEmpty) {
          // could get an empty model if mappedModels is a constant like `True`
          EmptyModel
        } else {
          mappedModels.toMap
        }
      }
  }
}
