package scala
package reflect
package internal
package tpe

import scala.collection.{ generic }
import generic.Clearable
import util.Origins

private[internal] trait TypeConstraints {
  self: SymbolTable =>
  import definitions._

  /** A log of type variable with their original constraints. Used in order
    *  to undo constraints in the case of isSubType/isSameType failure.
    */
  lazy val undoLog = newUndoLog

  protected def newUndoLog = new UndoLog

  class UndoLog extends Clearable {
    private type UndoPairs = List[(TypeVar, TypeConstraint)]
    //OPT this method is public so we can do `manual inlining`
    var log: UndoPairs = List()
    var size: Int = 0
    private def pop() = {
      val res = log.head
      log = log.tail
      size -= 1
      res
    }

    /*
     * These two methods provide explicit locking mechanism that is overridden in SynchronizedUndoLog.
     *
     * The idea behind explicit locking mechanism is that all public methods that access mutable state
     * will have to obtain the lock for their entire execution so both reads and writes can be kept in
     * right order. Originally, that was achieved by overriding those public methods in
     * `SynchronizedUndoLog` which was fine but expensive. The reason is that those public methods take
     * thunk as argument and if we keep them non-final there's no way to make them inlined so thunks
     * can go away.
     *
     * By using explicit locking we can achieve inlining.
     *
     * NOTE: They are made public for now so we can apply 'manual inlining' (copy&pasting into hot
     * places implementation of `undo` or `undoUnless`). This should be changed back to protected
     * once inliner is fixed.
     */
    def lock(): Unit = ()
    def unlock(): Unit = ()

    // register with the auto-clearing cache manager
    perRunCaches.recordCache(this)

    /** Undo all changes to constraints to type variables upto `limit`. */
    //OPT this method is public so we can do `manual inlining`
    def undoTo(limit: Int) {
      if (limit != size)
        self.log(s"[undo] log has $size/${log.size} entries, undo to $limit")

      assertCorrectThread()
      while (size > limit) {
        val (tv, constr) = pop()
        self.log(s"[undo] $tv.constr = $constr")
        tv.constr = constr
      }
    }
    def dropTo(limit: Int) {
      if (limit != size)
        self.log(s"[undo] log has $size/${log.size} entries, drop to $limit")

      while (size > limit)
        pop()
    }

    /** No sync necessary, because record should only
      *  be called from within an undo or undoUnless block,
      *  which is already synchronized.
      */
    private[reflect] def record(tv: TypeVar) = Origins("record", 4) {
      // self.log(s"($tv)")
      log ::= logResult(s"[undo+] ")(((tv, tv.constr.cloneInternal)))
      size += 1
    }

    def clear() {
      lock()
      self.log(s"Clearing ${log.size} entries from the undoLog.")
      log.zipWithIndex foreach { case ((k, v), i) =>
        def loop(xs: List[Symbol]): List[Symbol] = xs match {
          case Nil                 => Nil
          case c :: _ if c.isClass => c :: Nil
          case c :: cs             => c :: loop(cs)
        }
        val os = loop(k.origin.typeSymbolDirect.ownerChain)
        // val os = k.origin.typeSymbolDirect.ownerChain

        // val o1 = k.origin.typeSymbolDirect
        // val o2 = o1.enclMethod orElse o1.safeOwner
        // val o3 = o1.enclClass orElse o1.safeOwner
        // val c_s = List(o1, o2, o3).distinct filterNot (_ eq NoSymbol)
        val c_s = os map (s =>
          if (s.isMethod) s.defString
          else if (s.isClass) s.fullNameString
          else s.nameString
        ) mkString " in "

        // val c_s = k.origin.typeSymbolDirect.enclMethod match {
        //   case NoSymbol => k.origin.typeSymbolDirect + " in " + k.origin.typeSymbolDirect.safeOwner
        //   case m        => m.defString + " in " + m.enclClass.fullNameString
        // }
        // val m_s  = k.origin.typeSymbolDirect.enclMethod
        // val c_s  = m_s.enclClass.fullNameString
        // val mc_s = s"$c_s#${m_s.defString}"
        self.log(f"$i%3s) $k%30s -> $v%-20s ($c_s)")
      }
      try { log = Nil ; size = 0 } finally unlock()
    }

    // `block` should not affect constraints on typevars
    def undo[T](block: => T): T = Origins("undo", 4) {
      self.log(s"Undolog.undo(${log.size} entries)")
      lock()
      try {
        val before = size

        try block
        finally undoTo(before)
      } finally unlock()
    }
  }

  /** @PP: Unable to see why these apparently constant types should need vals
    *  in every TypeConstraint, I lifted them out.
    */
  private lazy val numericLoBound = IntClass.tpe
  private lazy val numericHiBound = intersectionType(List(ByteClass.tpe, CharClass.tpe), ScalaPackageClass)

  /** A class expressing upper and lower bounds constraints of type variables,
    * as well as their instantiations.
    */
  class TypeConstraint(lo0: List[Type], hi0: List[Type], numlo0: Type, numhi0: Type, avoidWidening0: Boolean = false) {
    def this(lo0: List[Type], hi0: List[Type]) = this(lo0, hi0, NoType, NoType)
    def this(bounds: TypeBounds) = this(List(bounds.lo), List(bounds.hi))
    def this() = this(List(), List())

    /*  Syncnote: Type constraints are assumed to be used from only one
     *  thread. They are not exposed in api.Types and are used only locally
     *  in operations that are exposed from types. Hence, no syncing of any
     *  variables should be ncessesary.
     */

    /** Guard these lists against AnyClass and NothingClass appearing,
      *  else loBounds.isEmpty will have different results for an empty
      *  constraint and one with Nothing as a lower bound.  [Actually
      *  guarding addLoBound/addHiBound somehow broke raw types so it
      *  only guards against being created with them.]
      */
    private var lobounds = lo0 //filterNot typeIsNothing
    private var hibounds = hi0 //filterNot typeIsAny
    private var numlo = numlo0
    private var numhi = numhi0
    private var avoidWidening = avoidWidening0

    def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    def avoidWiden: Boolean = avoidWidening

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      // For some reason which is still a bit fuzzy, we must let Nothing through as
      // a lower bound despite the fact that Nothing is always a lower bound.  My current
      // supposition is that the side-effecting type constraint accumulation mechanism
      // depends on these subtype tests being performed to make forward progress when
      // there are mutally recursive type vars.
      // See pos/t6367 and pos/t6499 for the competing test cases.
      val mustConsider = tp.typeSymbol.isNothingClass || !(lobounds contains tp)

      if (mustConsider) {
        if (isNumericBound && isNumericValueType(tp)) {
          if (numlo == NoType || isNumericSubType(numlo, tp))
            numlo = tp
          else if (!isNumericSubType(tp, numlo))
            numlo = numericLoBound
        }
        else lobounds ::= tp
      }
    }

    def checkWidening(tp: Type) {
      if(tp.isStable) avoidWidening = true
      else tp match {
        case HasTypeMember(_, _) => avoidWidening = true
        case _ =>
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
      // My current test case only demonstrates the need to let Nothing through as
      // a lower bound, but I suspect the situation is symmetrical.
      val mustConsider = tp.typeSymbol match {
        case AnyClass => true
        case _        => !(hibounds contains tp)
      }
      if (mustConsider) {
        checkWidening(tp)
        if (isNumericBound && isNumericValueType(tp)) {
          if (numhi == NoType || isNumericSubType(tp, numhi))
            numhi = tp
          else if (!isNumericSubType(numhi, tp))
            numhi = numericHiBound
        }
        else hibounds ::= tp
      }
    }

    def isWithinBounds(tp: Type): Boolean =
      lobounds.forall(_ <:< tp) &&
        hibounds.forall(tp <:< _) &&
        (numlo == NoType || (numlo weak_<:< tp)) &&
        (numhi == NoType || (tp weak_<:< numhi))

    var inst: Type = NoType // @M reduce visibility?
    private def instString = if (inst eq null) "null" else inst.safeToString

    def instValid = (inst ne null) && (inst ne NoType)

    def cloneInternal = {
      val tc = new TypeConstraint(lobounds, hibounds, numlo, numhi, avoidWidening)
      tc.inst = inst
      tc
    }

    override def toString = {
      val boundsStr = {
        val lo    = loBounds filterNot typeIsNothing
        val hi    = hiBounds filterNot typeIsAny
        val lostr = if (lo.isEmpty) Nil else List(lo.mkString(" >: (", ", ", ")"))
        val histr = if (hi.isEmpty) Nil else List(hi.mkString(" <: (", ", ", ")"))

        lostr ++ histr mkString ("[", " | ", "]")
      }
      if (inst eq NoType) boundsStr
      else s"$boundsStr _= $instString"
    }
  }

  /** Solve constraint collected in types `tvars`.
    *
    *  @param tvars      All type variables to be instantiated.
    *  @param tparams    The type parameters corresponding to `tvars`
    *  @param variances  The variances of type parameters; need to reverse
    *                    solution direction for all contravariant variables.
    *  @param upper      When `true` search for max solution else min.
    */
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Variance], upper: Boolean): Boolean =
    solve(tvars, tparams, variances, upper, AnyDepth)

  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Variance], upper: Boolean, depth: Int): Boolean = {

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Variance) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance.isContravariant) !upper else upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        //Console.println("solveOne0(tv, tp, v, b)="+(tvar, tparam, variance, bound))
        var cyclic = bound contains tparam
        foreach3(tvars, tparams, variances)((tvar2, tparam2, variance2) => {
          val ok = (tparam2 != tparam) && (
            (bound contains tparam2)
              ||  up && (tparam2.info.bounds.lo =:= tparam.tpeHK)
              || !up && (tparam2.info.bounds.hi =:= tparam.tpeHK)
            )
          if (ok) {
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        })
        def inst(tp: Type): Type = tp.instantiateTypeParams(tparams, tvars)

        if (!cyclic) {
          if (up) {
            if (bound.typeSymbol != AnyClass) {
              log(s"$tvar addHiBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addHiBound inst(bound)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.lo.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addHiBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addHiBound inst(tparam2.tpeHK)
                case _ =>
              }
          }
          else {
            if (!bound.typeSymbol.isNothingClass && bound.typeSymbol != tparam) {
              log(s"$tvar addLoBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addLoBound inst(bound)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.hi.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addLoBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addLoBound inst(tparam2.tpeHK)
                case _ =>
              }
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar

        //println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen)))
        val newInst = (
          if (up) {
            if (depth != AnyDepth) glb(tvar.constr.hiBounds, depth) else glb(tvar.constr.hiBounds)
          } else {
            if (depth != AnyDepth) lub(tvar.constr.loBounds, depth) else lub(tvar.constr.loBounds)
          }
          )
        log(s"$tvar setInst $newInst")
        tvar setInst newInst
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen))+" = "+tvar.constr.inst)//@MDEBUG
      }
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)
    tvars forall (tvar => tvar.constr.isWithinBounds(tvar.constr.inst))
  }
}
