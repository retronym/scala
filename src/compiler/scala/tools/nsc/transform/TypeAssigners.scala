/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._

trait TypeAssigners { self: TypingTransformers =>
  import global._
  import definitions._

  /** Types trees after typer whose symbols are already bound, from the infos of their symbols
   *  and the types of their children, as `Typer` would type them, but without name resolution,
   *  inference, implicit search, overloading resolution, access checks or `silent`.
   *
   *  Tree shapes outside that grammar, or that would need any of those, go to `localTyper`
   *  (`fallback`), with a dev warning.
   *
   *  A subclass hooks in at three points: `enter` (on entering an original tree), `adapt`
   *  (adapting a typed tree to its expected type) and `typed1`/`typedCore` (special cases).
   *  `Erasure.ErasureRetyper` is one, typing erased trees.
   */
  trait TypeAssigner extends TypingTransformer {
    /** The result type of the enclosing method, the expected type of `Return` expressions. */
    private[this] var enclMethodResult: Type = NoType

    /** Called on entering a tree, before looking at its type. */
    protected def enter(tree: Tree): Tree = tree

    /** Adapts a typed tree to its expected type. Before erasure, as `Typer.adapt` does for the
     *  trees the assigner types: a reference to a nullary method gets the method's result type,
     *  and an expression with a constant type becomes a literal. `funMode` is set for the
     *  function of an application.
     */
    protected def adapt(tree: Tree, pt: Type, funMode: Boolean): Tree =
      if (phase.erasedTypes) tree
      else if (funMode) skolemized(tree)
      else tree.tpe match {
        case ct @ FoldableConstantType(value) if ct <:< pt =>
          tree match {
            case Literal(`value`) => tree
            case _ => treeCopy.Literal(tree, value).updateAttachment(analyzer.OriginalTreeAttachment(tree))
          }
        case NullaryMethodType(restpe) =>
          val sym = tree.symbol
          // `Typer.adapt` keeps a constant type only where it is safe to fold.
          val restpe1 =
            if (sym.isAccessor && sym.hasFlag(scala.reflect.internal.Flags.STABLE) && treeInfo.isExprSafeToInline(tree)) restpe
            else restpe.deconst
          adapt(tree setType restpe1, pt, funMode)
        case _ => skolemized(tree)
      }

    /** An expression of an existential type gets its skolemised type, as in `Typer.adapt`, except
     *  in the body of an accessor (`Typer.isSyntheticAccessor`, past typer).
     */
    private def skolemized(tree: Tree): Tree = {
      val tp = tree.tpe
      if (tree.isTerm && isExistentialType(tp) && !localTyper.context.owner.isAccessor)
        tree setType tp.dealias.skolemizeExistential(localTyper.context.owner, tree)
      else tree
    }

    /** Whether a value of type `tp` can be passed where `formal` is expected, without conversion. */
    private def conforms(tp: Type, formal: Type): Boolean =
      (tp eq formal) || isRepeatedParamType(formal) || tp <:< dropByName(formal)

    /** Prepares a tree that `fallback` hands to `localTyper`. */
    protected def beforeFallback(tree: Tree): Tree = tree

    /** A tree shape the assigner does not know: type it with `localTyper`. */
    protected def fallback(tree: Tree, pt: Type, why: String): Tree = {
      devWarning(tree.pos, s"$phase: typing $why with ${localTyper.getClass.getSimpleName}")
      localTyper.typed(beforeFallback(tree), pt)
    }

    /** The typer's mode for an expression, or for the qualifier of a selection. */
    protected final def exprMode(qualMode: Boolean): Mode = if (qualMode) EXPRmode | QUALmode else EXPRmode

    /** The type of a conditional, match or try whose branches have types `tpes`: `pt` if it is
     *  fully defined, else the branches' lub, or their weak lub if that differs, as in
     *  `Typer.typedIf`.
     */
    private def branchesType(pt: Type, tpes: => List[Type]): Type =
      if (analyzer.isFullyDefined(pt)) pt
      else {
        val tpes1 = tpes.map(_.deconst)
        if (glbLubs.sameWeakLubAsLub(tpes1)) lub(tpes1) else glbLubs.weakLub(tpes1)
      }

    /** Analogue of `Typer.typed`. */
    def typed(tree: Tree, pt: Type): Tree = typedMode(tree, pt, qualMode = false, funMode = false)

    protected final def typedQualifier(tree: Tree, pt: Type = WildcardType): Tree = typedMode(tree, pt, qualMode = true, funMode = false)

    /** The function of an application. */
    protected final def typedFun(tree: Tree): Tree = typedMode(tree, WildcardType, qualMode = false, funMode = true)

    /** A tree with a type is not retyped. */
    private def typedMode(tree: Tree, pt: Type, qualMode: Boolean, funMode: Boolean): Tree = {
      val tree0 = enter(tree)
      val tree1 = if (tree0.tpe ne null) tree0 else typed1(tree0, pt, qualMode)
      if (tree1.isEmpty) tree1
      else if (!funMode && !phase.erasedTypes && tree1.tpe.isInstanceOf[OverloadedType])
        // An overloaded member selected by name and not applied, such as a name-based `isEmpty`:
        // `Typer.adapt` resolves it against `pt`.
        fallback(tree1, pt, "overloaded-expr")
      else adapt(tree1, pt, funMode)
    }

    /** Analogue of `Typer.typed1`. */
    protected def typed1(tree: Tree, pt: Type, qualMode: Boolean): Tree = typedCore(tree, pt, qualMode)

    /** A stable identifier or selection in a context that requires it gets its singleton type,
     *  as with `Typer.stabilize`. After erasure, as `ErasureRetyper` has it, it keeps its
     *  underlying type.
     */
    private def stabilized(tree: Tree, pre: Type, qualMode: Boolean, pt: Type): Tree =
      if (phase.erasedTypes) tree else localTyper.stabilize(tree, pre, exprMode(qualMode), pt)

    /** Analogue of `Typer.typed1` for the tree shapes that synthesised trees, and erased trees, have. */
    protected def typedCore(tree: Tree, pt: Type, qualMode: Boolean): Tree = tree match {
      case tree: Ident =>
        val sym = tree.symbol
        if (sym == NoSymbol) return fallback(tree, pt, "Ident-nosym")
        val pre = if (sym.isTopLevel) sym.owner.thisType else NoPrefix
        stabilized(tree setType pre.memberType(sym), pre, qualMode, pt)

      case sel @ Select(qual, _) =>
        typedSelect(sel, typedQualifier(qual), qualMode, pt)

      case Apply(blk @ Block(stats, expr), args) =>
        val ap1 = treeCopy.Apply(tree, expr, args).clearType().setPos(tree.pos.makeTransparent)
        typedCore(treeCopy.Block(blk, stats, ap1).clearType(), pt, qualMode)

      case Apply(fun, _) =>
        typedApply(tree.asInstanceOf[Apply], typedFun(fun), pt, qualMode)

      case ta @ TypeApply(fun, _) =>
        typedTypeApply(treeCopy.TypeApply(ta, typedFun(fun), ta.args), pt)

      case ReferenceToBoxed(ident) =>
        // Typer.typedReferenceToBoxed
        val ident1 = typed1(ident, pt, qualMode)
        treeCopy.ReferenceToBoxed(tree, ident1.asInstanceOf[Ident]) setType capturedVariables.capturedVariableType(ident.symbol, erasedTypes = phase.erasedTypes)

      case Literal(value) =>
        tree setType (if (value.tag == UnitTag) UnitTpe else ConstantType(value))

      case tree: This =>
        // Typer.typedThis
        tree.symbol orElse localTyper.qualifyingClass(tree, tree.qual, packageOK = false, immediate = true) match {
          case NoSymbol => tree setType ErrorType // reported by qualifyingClass
          case clazz =>
            tree setSymbol clazz setType clazz.thisType.underlying
            if (localTyper.isStableContext(tree, exprMode(qualMode), pt)) tree setType clazz.thisType else tree
        }

      case sup: Super =>
        typedSuper(sup)

      case Typed(expr, tpt0) =>
        // A `TypeTree` comes typed; another type tree is typed here.
        val tpt = if (tpt0.tpe eq null) typed(tpt0, WildcardType) else tpt0
        val expr1 = typed(expr, tpt.tpe.deconst)
        treeCopy.Typed(tree, expr1, tpt) setType tpt.tpe

      case Block(stats, expr) =>
        val stats1 = typedStats(stats)
        val expr1 = typed(expr, pt)
        val block1 = treeCopy.Block(tree, stats1, expr1)
        block1.setType(if (treeInfo.isExprSafeToInline(block1)) expr1.tpe else expr1.tpe.deconst)

      case If(cond, thenp, elsep) =>
        val cond1 = typed(cond, BooleanTpe)
        if (elsep.isEmpty)
          treeCopy.If(tree, cond1, typed(thenp, UnitTpe), elsep) setType UnitTpe
        else {
          val thenp1 = typed(thenp, pt)
          val elsep1 = typed(elsep, pt)
          treeCopy.If(tree, cond1, thenp1, elsep1) setType branchesType(pt, thenp1.tpe :: elsep1.tpe :: Nil)
        }

      case Match(selector, cases) =>
        val selector1 = typed(selector, WildcardType)
        val selectorTp = selector1.tpe.widen
        val cases1 = typedCases(cases, selectorTp, pt)
        treeCopy.Match(tree, selector1, cases1) setType branchesType(pt, cases1.map(_.tpe))

      case Try(block, catches, finalizer) =>
        val block1 = typed(block, pt)
        val catches1 = typedCases(catches, ThrowableTpe, pt)
        val fin1 = if (finalizer.isEmpty) finalizer else typed(finalizer, UnitTpe)
        treeCopy.Try(tree, block1, catches1, fin1) setType branchesType(pt, (block1 :: catches1).map(_.tpe))

      case Throw(expr) =>
        treeCopy.Throw(tree, typed(expr, ThrowableTpe)) setType NothingTpe

      case Return(expr) =>
        if (enclMethodResult eq NoType) return fallback(tree, pt, "Return-noencl")
        treeCopy.Return(tree, typed(expr, enclMethodResult)) setType NothingTpe

      case New(tpt) =>
        tree setType tpt.tpe

      case Assign(lhs, rhs) =>
        val lhs1 = typed(lhs, WildcardType)
        val varsym = lhs1.symbol
        if (varsym == null || treeInfo.mayBeVarGetter(varsym)) return fallback(treeCopy.Assign(tree, lhs1, rhs), pt, "Assign-setter")
        treeCopy.Assign(tree, lhs1, typed(rhs, lhs1.tpe)) setType UnitTpe

      case ArrayValue(elemtpt, elems) =>
        val elems1 = elems mapConserve (elem => typed(elem, elemtpt.tpe))
        treeCopy.ArrayValue(tree, elemtpt, elems1) setType arrayType(elemtpt.tpe)

      case ApplyDynamic(qual, args) =>
        val qual1 = typed(qual, AnyRefTpe)
        val args1 = args mapConserve (arg => typed(arg, AnyRefTpe))
        treeCopy.ApplyDynamic(tree, qual1, args1) setType AnyRefTpe

      case ldef @ LabelDef(_, params, rhs) =>
        val restpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(rhs, restpe)
        params foreach (param => param setType param.symbol.tpe)
        deriveLabelDef(ldef)(_ => rhs1) setType restpe

      case vdef @ ValDef(_, _, tpt, rhs) =>
        val sym = vdef.symbol
        val rhs1 =
          if (rhs.isEmpty) rhs
          else if (sym.hasDefault) return fallback(vdef, pt, "ValDef-default")
          else atOwner(vdef, sym)(typed(rhs, tpt.tpe))
        if (!phase.erasedTypes && !rhs1.isEmpty && !conforms(rhs1.tpe, tpt.tpe))
          return fallback(treeCopy.ValDef(vdef, vdef.mods, vdef.name, tpt, rhs1), pt, "ValDef-nonconforming")
        treeCopy.ValDef(vdef, vdef.mods, vdef.name, tpt, rhs1) setType NoType

      case ddef @ DefDef(_, _, tparams, vparamss, tpt, rhs) =>
        val meth = ddef.symbol
        val vparamss1 = vparamss.mapConserve(_.mapConserve(vd => typed(vd, WildcardType).asInstanceOf[ValDef]))
        val savedResult = enclMethodResult
        enclMethodResult = tpt.tpe
        val rhs1 = try atOwner(ddef, meth) {
          if (ddef.name == nme.CONSTRUCTOR && !meth.hasStaticFlag) typed(rhs, WildcardType)
          else typed(rhs, tpt.tpe)
        } finally enclMethodResult = savedResult
        treeCopy.DefDef(ddef, ddef.mods, ddef.name, tparams, vparamss1, tpt, rhs1) setType NoType

      case cdef @ ClassDef(_, _, tparams, impl) =>
        val savedResult = enclMethodResult
        enclMethodResult = NoType
        val impl1 = try atOwner(cdef, cdef.symbol)(typed(impl, WildcardType)).asInstanceOf[Template]
        finally enclMethodResult = savedResult
        treeCopy.ClassDef(cdef, cdef.mods, cdef.name, tparams, impl1) setType NoType

      case templ @ Template(parents, _, body) =>
        val clazz = currentOwner
        val body1 = atOwner(templ, clazz) {
          body mapConserve { stat =>
            if (stat.isDef) typed(stat, WildcardType)
            else atOwner(stat, templ.symbol)(typed(stat, WildcardType))
          }
        }
        treeCopy.Template(templ, parents, noSelfType, body1) setType clazz.tpe_*

      case pdef @ PackageDef(pid, stats) =>
        val pid1 = typedQualifier(pid).asInstanceOf[RefTree]
        val stats1 = atOwner(pdef, pdef.symbol)(stats mapConserve (stat => typed(stat, WildcardType)))
        treeCopy.PackageDef(pdef, pid1, stats1) setType NoType

      case fun @ Function(vparams, body) if !phase.erasedTypes =>
        // Typer.typed1 and Typer.doTypedFunction past typer, for a function with no expected
        // function or SAM type, which would give its body an expected type.
        if (pt ne WildcardType) return fallback(tree, pt, "Function-pt")
        if (fun.symbol == NoSymbol) fun.symbol = localTyper.context.owner.newAnonymousFunctionValue(fun.pos)
        val vparams1 = vparams.mapConserve(vd => typed(vd, WildcardType).asInstanceOf[ValDef])
        val body1 = atOwner(fun, fun.symbol)(typed(body, WildcardType))
        val resTp = localTyper.packedType(body1, fun.symbol).resultType.deconst match {
          case ct: ConstantType => ct.widen
          case tp => tp
        }
        val funTp = appliedType(FunctionClass(vparams.length), vparams.map(_.symbol.tpe) :+ resTp)
        treeCopy.Function(fun, vparams1, body1) setType funTp

      case _ =>
        fallback(tree, pt, "core-" + tree.getClass.getSimpleName)
    }

    /** `sel` on its typed qualifier `qual1`: `Typer.typedSelect`. */
    protected final def typedSelect(sel: Select, qual1: Tree, qualMode: Boolean, pt: Type): Tree = {
      val sym = sel.symbol orElse qual1.tpe.member(sel.name)
      if (sym == NoSymbol) return fallback(treeCopy.Select(sel, qual1, sel.name), pt, "Select-nosym")
      if (sym.alternatives.head.owner.isPackageObjectClass && qual1.symbol != null && qual1.symbol.hasPackageFlag)
        // As `Typer.typedSelect`, select a package object's member from the package object.
        return typedSelect(sel, typedQualifier(atPos(qual1.pos.focus)(Select(qual1, nme.PACKAGE))), qualMode, pt)
      if (qual1.isInstanceOf[Super]) {
        // A selection from `super` is typed against the member's owner, or the accessible class
        // in the `QualTypeSymAttachment` (scala/bug#7936): see the comment in `Erasure.preErase`.
        val qualSym = sel.getAndRemoveAttachment[QualTypeSymAttachment] match {
          case Some(a) => a.sym
          case None => sym.owner
        }
        qual1.setType(qualSym.tpe)
      }
      // An overloaded member, as `Typer.makeAccessible` types it, for `typedApply` to resolve.
      val owntype = if (sym.isOverloaded) OverloadedType(qual1.tpe, sym.alternatives) else qual1.tpe.memberType(sym)
      val sel1 = treeCopy.Select(sel, qual1, sel.name).setSymbol(sym).setType(owntype)
      if (sym.isOverloaded) sel1 else stabilized(sel1, qual1.tpe, qualMode, pt)
    }

    /** `app` with its typed function `fun1`: types the arguments against the parameter types. */
    protected final def typedApply(app: Apply, fun1: Tree, pt: Type, qualMode: Boolean): Tree = {
      // Typer.normalTypedApply
      val fun2 = {
        val sym = fun1.symbol
        if ((sym ne null) && sym.isMethod && sym.isStable) localTyper.stabilizeFun(fun1, exprMode(qualMode), pt)
        else fun1
      }
      def typedArgs = app.args.mapConserve(typed(_, WildcardType))
      fun2.tpe match {
        case _ if treeInfo.isWildcardStarArgList(app.args) =>
          fallback(treeCopy.Apply(app, fun2, app.args), pt, "Apply-star")
        case mt @ MethodType(params, _) if sameLength(params, app.args) =>
          val args1 = map2Conserve(app.args, params)((arg, param) => typed(arg, param.tpe))
          // Before erasure, an argument that does not conform needs the typer's conversions, or
          // its error (a translated match can call an extractor's members with the wrong types).
          if (!phase.erasedTypes && !args1.corresponds(params)((arg, param) => conforms(arg.tpe, param.tpe)))
            return fallback(treeCopy.Apply(app, fun2, args1), pt, "Apply-nonconforming")
          treeCopy.Apply(app, fun2, args1) setType mt.resultType(args1.map(_.tpe))
        case OverloadedType(pre, alts) if !phase.erasedTypes =>
          // An operator selected by name, such as `==` or `&` on a primitive: the alternative
          // whose parameter types are those of the arguments, if there is exactly one.
          val args1 = typedArgs
          val argTypes = args1.map(_.tpe.widen)
          def matches(alt: Symbol) = pre.memberType(alt) match {
            case MethodType(params, _) => params.corresponds(argTypes)((p, a) => p.tpe =:= a)
            case _ => false
          }
          alts.filter(matches) match {
            case alt :: Nil =>
              val fun3 = fun2.setSymbol(alt).setType(pre.memberType(alt))
              typedApply(treeCopy.Apply(app, fun3, args1), fun3, pt, qualMode)
            case _ =>
              fallback(treeCopy.Apply(app, fun2, args1), pt, "Apply-overloaded")
          }
        case PolyType(tparams, MethodType(params, _)) if !phase.erasedTypes && sameLength(params, app.args) &&
            tparams.forall(tparam => params.exists(_.tpe.typeSymbolDirect == tparam)) =>
          // A polymorphic method such as `synchronized`, each of whose type parameters is the
          // type of a parameter: instantiated to the widened types of the arguments, as
          // `Infer.inferMethodInstance` does for them.
          val args1 = typedArgs
          val targs = tparams.map(tparam => args1(params.indexWhere(_.tpe.typeSymbolDirect == tparam)).tpe.widen)
          val fun3 = typedTypeApply(TypeApply(fun2, targs.map(TypeTree(_))).setPos(fun2.pos), pt)
          typedApply(treeCopy.Apply(app, fun3, args1), fun3, pt, qualMode)
        case _ =>
          fallback(treeCopy.Apply(app, fun2, app.args), pt, "Apply-" + fun2.tpe.getClass.getSimpleName)
      }
    }

    /** `ta` with its typed function: instantiates the polymorphic type. */
    protected final def typedTypeApply(ta: TypeApply, pt: Type): Tree = ta.fun.tpe match {
      case PolyType(tparams, restpe) if tparams.nonEmpty && sameLength(tparams, ta.args) =>
        ta setType restpe.instantiateTypeParams(tparams, ta.args.map(_.tpe))
      case _ =>
        fallback(ta, pt, "TypeApply-nonpoly")
    }

    private def typedSuper(tree: Super): Tree = {
      val qual1 = typed(tree.qual, WildcardType)
      val clazz = qual1 match {
        case This(_) => qual1.symbol
        case _ => qual1.tpe.typeSymbol
      }
      val owntype =
        if (!tree.mix.isEmpty) clazz.tpe.parents.find(_.typeSymbol.name == tree.mix) match {
          case Some(parent) => parent
          case None => return fallback(tree, WildcardType, "Super-mix")
        }
        else intersectionType(clazz.info.parents)
      treeCopy.Super(tree, qual1, tree.mix) setType SuperType(clazz.thisType, owntype)
    }

    private def typedStats(stats: List[Tree]): List[Tree] =
      stats mapConserve (stat => typed(stat, WildcardType))

    private def typedCases(cases: List[CaseDef], pattpe: Type, pt: Type): List[CaseDef] =
      cases mapConserve { cdef =>
        val pat1 = typedPattern(cdef.pat, pattpe)
        val guard1 = if (cdef.guard.isEmpty) cdef.guard else typed(cdef.guard, BooleanTpe)
        val body1 = typed(cdef.body, pt)
        treeCopy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
      }

    /** Patterns that survive the pattern matcher: switch alternatives and catch patterns. */
    private def typedPattern(pat0: Tree, pt: Type): Tree = {
      val pat = enter(pat0)
      pat match {
        case Ident(nme.WILDCARD) => pat setType pt
        case Literal(value) => pat setType ConstantType(value)
        case Alternative(alts) => treeCopy.Alternative(pat, alts mapConserve (typedPattern(_, pt))) setType pt
        case Typed(expr, tpt) =>
          val expr1 = typedPattern(expr, tpt.tpe)
          treeCopy.Typed(pat, expr1, tpt) setType tpt.tpe
        case Bind(name, body) =>
          val body1 = typedPattern(body, pt)
          pat.symbol.setInfo(body1.tpe)
          treeCopy.Bind(pat, name, body1) setType body1.tpe
        case _ =>
          devWarning(pat.pos, s"$phase: typing pattern ${pat.getClass.getSimpleName} with ${localTyper.getClass.getSimpleName}")
          localTyper.typedPattern(beforeFallback(pat), pt)
      }
    }
  }

  /** A `TypeAssigner` typing in the context of `typer`. */
  final class TypeAssignerIn(typer: analyzer.Typer) extends TypingTransformer(typer) with TypeAssigner {
    def typedStable(tree: Tree): Tree = typedQualifier(tree)
  }

  /** `tree`, synthesised after typer, typed with a `TypeAssigner` in the context of `typer`. */
  def assignTypes(typer: analyzer.Typer, tree: Tree, pt: Type = WildcardType): Tree =
    new TypeAssignerIn(typer).typed(tree, pt)
}
