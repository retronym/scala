package scala.reflect
package internal
package types

import scala.collection.{ mutable, immutable, generic }
import generic.Clearable
import scala.ref.WeakReference
import mutable.ListBuffer
import Flags._
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import util.Statistics
import scala.runtime.ObjectRef
import util.ThreeValues._
import Variance._

trait TypeMaps {
  self: SymbolTable =>
  import definitions._
  // todo. move these into scala.reflect.api

  /** A prototype for mapping a function over all possible types
    */
  abstract class TypeMap(trackVariance: Boolean) extends (Type => Type) {
    def this() = this(trackVariance = false)
    def apply(tp: Type): Type

    private[this] var _variance: Variance = if (trackVariance) Covariant else Invariant

    def variance_=(x: Variance) = { assert(trackVariance, this) ; _variance = x }
    def variance = _variance

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 = (
          if (trackVariance && args.nonEmpty && !variance.isInvariant && sym.typeParams.nonEmpty)
            mapOverArgs(args, sym.typeParams)
          else
            args mapConserve this
          )
        if ((pre1 eq pre) && (args1 eq args)) tp
        else copyTypeRef(tp, pre1, tr.coevolveSym(pre1), args1)
      case ThisType(_) => tp
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if (pre1 eq pre) tp
          else singleType(pre1, sym)
        }
      case MethodType(params, result) =>
        val params1 = flipped(mapOver(params))
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        val tparams1 = flipped(mapOver(tparams))
        val result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case NullaryMethodType(result) =>
        val result1 = this(result)
        if (result1 eq result) tp
        else NullaryMethodType(result1)
      case ConstantType(_) => tp
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp)
        val supertp1 = this(supertp)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else SuperType(thistp1, supertp1)
      case TypeBounds(lo, hi) =>
        val lo1 = flipped(this(lo))
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case BoundedWildcardType(bounds) =>
        val bounds1 = this(bounds)
        if (bounds1 eq bounds) tp
        else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
      case rtp @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve this
        val decls1 = mapOver(decls)
        copyRefinedType(rtp, parents1, decls1)
      case ExistentialType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        val result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else newExistentialType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else this(pre)
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case AntiPolyType(pre, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve this
        if ((pre1 eq pre) && (args1 eq args)) tp
        else AntiPolyType(pre1, args1)
      case tv@TypeVar(_, constr) =>
        if (constr.instValid) this(constr.inst)
        else tv.applyArgs(mapOverArgs(tv.typeArgs, tv.params))  //@M !args.isEmpty implies !typeParams.isEmpty
      case NotNullType(tp) =>
        val tp1 = this(tp)
        if (tp1 eq tp) tp
        else NotNullType(tp1)
      case AnnotatedType(annots, atp, selfsym) =>
        val annots1 = mapOverAnnotations(annots)
        val atp1 = this(atp)
        if ((annots1 eq annots) && (atp1 eq atp)) tp
        else if (annots1.isEmpty) atp1
        else AnnotatedType(annots1, atp1, selfsym)
      /*
            case ErrorType => tp
            case WildcardType => tp
            case NoType => tp
            case NoPrefix => tp
            case ErasedSingleType(sym) => tp
      */
      case _ =>
        tp
      // throw new Error("mapOver inapplicable for " + tp);
    }

    def withVariance[T](v: Variance)(body: => T): T = {
      val saved = variance
      variance = v
      try body finally variance = saved
    }
    @inline final def flipped[T](body: => T): T = {
      if (trackVariance) variance = variance.flip
      try body
      finally if (trackVariance) variance = variance.flip
    }
    protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] = (
      if (trackVariance)
        map2Conserve(args, tparams)((arg, tparam) => withVariance(variance * tparam.variance)(this(arg)))
      else
        args mapConserve this
      )
    /** Applies this map to the symbol's info, setting variance = Invariant
      *  if necessary when the symbol is an alias.
      */
    private def applyToSymbolInfo(sym: Symbol): Type = {
      if (trackVariance && !variance.isInvariant && sym.isAliasType)
        withVariance(Invariant)(this(sym.info))
      else
        this(sym.info)
    }

    /** Called by mapOver to determine whether the original symbols can
      *  be returned, or whether they must be cloned.
      */
    protected def noChangeToSymbols(origSyms: List[Symbol]): Boolean = {
      @tailrec def loop(syms: List[Symbol]): Boolean = syms match {
        case Nil     => true
        case x :: xs => (x.info eq applyToSymbolInfo(x)) && loop(xs)
      }
      loop(origSyms)
    }

    /** Map this function over given scope */
    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    /** Map this function over given list of symbols */
    def mapOver(origSyms: List[Symbol]): List[Symbol] = {
      // fast path in case nothing changes due to map
      if (noChangeToSymbols(origSyms)) origSyms
      // map is not the identity --> do cloning properly
      else cloneSymbolsAndModify(origSyms, TypeMap.this)
    }

    def mapOver(annot: AnnotationInfo): AnnotationInfo = {
      val AnnotationInfo(atp, args, assocs) = annot
      val atp1  = mapOver(atp)
      val args1 = mapOverAnnotArgs(args)
      // there is no need to rewrite assocs, as they are constants

      if ((args eq args1) && (atp eq atp1)) annot
      else if (args1.isEmpty && args.nonEmpty) UnmappableAnnotation  // some annotation arg was unmappable
      else AnnotationInfo(atp1, args1, assocs) setPos annot.pos
    }

    def mapOverAnnotations(annots: List[AnnotationInfo]): List[AnnotationInfo] = {
      val annots1 = annots mapConserve mapOver
      if (annots1 eq annots) annots
      else annots1 filterNot (_ eq UnmappableAnnotation)
    }

    /** Map over a set of annotation arguments.  If any
      *  of the arguments cannot be mapped, then return Nil.  */
    def mapOverAnnotArgs(args: List[Tree]): List[Tree] = {
      val args1 = args mapConserve mapOver
      if (args1 contains UnmappableTree) Nil
      else args1
    }

    def mapOver(tree: Tree): Tree =
      mapOver(tree, () => return UnmappableTree)

    /** Map a tree that is part of an annotation argument.
      *  If the tree cannot be mapped, then invoke giveup().
      *  The default is to transform the tree with
      *  TypeMapTransformer.
      */
    def mapOver(tree: Tree, giveup: ()=>Nothing): Tree =
      (new TypeMapTransformer).transform(tree)

    /** This transformer leaves the tree alone except to remap
      *  its types. */
    class TypeMapTransformer extends Transformer {
      override def transform(tree: Tree) = {
        val tree1 = super.transform(tree)
        val tpe1 = TypeMap.this(tree1.tpe)
        if ((tree eq tree1) && (tree.tpe eq tpe1))
          tree
        else
          tree1.shallowDuplicate.setType(tpe1)
      }
    }
  }

  abstract class TypeTraverser extends TypeMap {
    def traverse(tp: Type): Unit
    def apply(tp: Type): Type = { traverse(tp); tp }
  }

  abstract class TypeTraverserWithResult[T] extends TypeTraverser {
    def result: T
    def clear(): Unit
  }

  abstract class TypeCollector[T](initial: T) extends TypeTraverser {
    var result: T = _
    def collect(tp: Type) = {
      result = initial
      traverse(tp)
      result
    }
  }
}
