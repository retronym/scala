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

  /** Types trees after typer whose symbols are already bound, from the infos of their symbols
   *  and the types of their children, as `Typer` would type them, but without name resolution,
   *  inference, implicit search, overloading resolution, access checks or `silent`.
   *
   *  Tree shapes outside that grammar, or that would need any of those, go to `localTyper`
   *  (`fallback`), with a dev warning.
   *
   *  Its methods have the names and signatures of the `Typer` methods they stand in for. The only
   *  modes are those of an expression, a qualifier and the function of an application.
   *
   *  A subclass hooks in at three points: `enter` (on entering an original tree), `adapt`
   *  (adapting a typed tree to its expected type) and `typed1`/`typedCore` (special cases).
   *  `Erasure.ErasureRetyper` is one, typing erased trees.
   *
   *  It needs no typing context: what it needs from `localTyper` goes through `enclosingOwner` and
   *  `fallbackTyper`, which `TypingTransformer` implements with its `localTyper`. After erasure, only
   *  `fallback` needs a `Typer`.
   *
   *  A class, not a trait, and with `global` in a field of its own: a dozen transformers extend it, so calls
   *  from its shared code to its own methods, to its enclosing phase or to `global` through an interface would
   *  dispatch through an itable at a megamorphic call site.
   */
  abstract class TypeAssigner extends OwnerStackTransformer {
    protected[this] final val assignerGlobal: TypeAssigners.this.global.type = TypeAssigners.this.global
    import assignerGlobal._
    import assignerGlobal.definitions._

    /** The innermost owner entered, as the owner of a typing context there would be: a module's class,
     *  not the module. */
    protected def enclosingOwner: Symbol = if (frames > 0) frameOwners(frames - 1).asInstanceOf[Symbol] else currentOwner

    private[this] var rootTyper: analyzer.Typer = null

    /** A `Typer` for what the assigner cannot do itself: one in the root context of the unit,
     *  made when first needed, with no context for the owners entered.
     */
    protected def fallbackTyper: analyzer.Typer = {
      if (rootTyper eq null) rootTyper = newRootLocalTyper(currentUnit)
      rootTyper
    }

    /** The result type of the enclosing method, the expected type of `Return` expressions. */
    private[this] var enclMethodResult: Type = NoType

    /** Called on entering a tree, before looking at its type. */
    protected def enter(tree: Tree): Tree = tree

    /** Adapts a typed tree to its expected type. Before erasure, as `Typer.adapt` does for the
     *  trees the assigner types: a reference to a nullary method gets the method's result type,
     *  and an expression with a constant type becomes a literal.
     */
    protected def adapt(tree: Tree, mode: Mode, pt: Type): Tree =
      if (phase.erasedTypes) tree
      else if (mode.inFunMode) skolemized(tree)
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
          adapt(tree setType restpe1, mode, pt)
        case _ => skolemized(tree)
      }

    /** An expression of an existential type gets its skolemised type, as in `Typer.adapt`, except
     *  in the body of an accessor (`Typer.isSyntheticAccessor`, past typer).
     */
    private def skolemized(tree: Tree): Tree = {
      val tp = tree.tpe
      if (tree.isTerm && isExistentialType(tp) && !enclosingOwner.isAccessor)
        tree setType tp.dealias.skolemizeExistential(enclosingOwner, tree)
      else tree
    }

    /** Whether a value of type `tp` can be passed where `formal` is expected, without conversion. */
    private def conforms(tp: Type, formal: Type): Boolean =
      (tp eq formal) || isRepeatedParamType(formal) || tp <:< dropByName(formal)

    /** Prepares a tree that `fallback` hands to `localTyper`. */
    protected def beforeFallback(tree: Tree): Tree = tree

    /** A tree shape the assigner does not know: type it with `localTyper`. */
    protected def fallback(tree: Tree, pt: Type, why: String): Tree = {
      val localTyper = fallbackTyper
      devWarning(tree.pos, s"$phase: typing $why with ${localTyper.getClass.getSimpleName}")
      localTyper.typed(beforeFallback(tree), pt)
    }

    /** `Typer.packedType(tree, owner)`, which changes only a type that mentions a symbol defined in `owner`,
     *  a captured existential, a type parameter of a raw type or an annotation.
     */
    private def packedType(tree: Tree, owner: Symbol): Type = {
      def mayPack(sym: Symbol) = (sym ne NoSymbol) &&
        (sym.hasTransOwner(owner) || sym.hasAllFlags(symtab.Flags.EXISTENTIAL | symtab.Flags.CAPTURED) || existentialsAndSkolems.isRawParameter(sym))
      if (tree.tpe.exists(t => t.isInstanceOf[AnnotatedType] || mayPack(t.typeSymbol) || mayPack(t.termSymbol)))
        fallbackTyper.packedType(tree, owner)
      else tree.tpe
    }

    /** The type of a conditional, match or try whose branches have types `tpes`: `pt` if it is
     *  fully defined, else the branches' lub, or their weak lub if that differs, as in
     *  `Typer.typedIf`.
     */
    @inline private def branchesType(pt: Type, tpes: => List[Type]): Type =
      if (analyzer.isFullyDefined(pt)) pt
      else {
        val tpes1 = tpes.map(_.deconst)
        if (glbLubs.sameWeakLubAsLub(tpes1)) lub(tpes1) else glbLubs.weakLub(tpes1)
      }

    /** Analogue of `Typer.typed`. A tree with a type is not retyped. */
    def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      val tree0 = enter(tree)
      val tree1 = if (tree0.tpe ne null) tree0 else typed1(tree0, mode, pt)
      if (tree1.isEmpty) tree1
      else if (!mode.inFunMode && !phase.erasedTypes && tree1.tpe.isInstanceOf[OverloadedType])
        // An overloaded member selected by name and not applied, such as a name-based `isEmpty`:
        // `Typer.adapt` resolves it against `pt`.
        fallback(tree1, pt, "overloaded-expr")
      else adapt(tree1, mode, pt)
    }

    def typed(tree: Tree, pt: Type): Tree = typed(tree, EXPRmode, pt)
    def typed(tree: Tree): Tree = typed(tree, EXPRmode, WildcardType)
    def typedPos(pos: Position)(tree: Tree): Tree = typed(atPos(pos)(tree))

    protected final def typedQualifier(tree: Tree, pt: Type): Tree = typed(tree, PolyQualifierModes, pt)
    protected final def typedQualifier(tree: Tree): Tree = typedQualifier(tree, WildcardType)

    /** The function of an application. */
    protected final def typedOperator(tree: Tree): Tree = typed(tree, OperatorModes, WildcardType)

    /** Analogue of `Typer.typed1`. */
    protected def typed1(tree: Tree, mode: Mode, pt: Type): Tree = typedCore(tree, mode, pt)

    /** A stable identifier or selection in a context that requires it gets its singleton type,
     *  as with `Typer.stabilize`. After erasure, as `Eraser` has it, it keeps its underlying type.
     */
    private def stabilize(tree: Tree, pre: Type, mode: Mode, pt: Type): Tree =
      if (phase.erasedTypes) tree
      else {
        val sym = tree.symbol
        // Without these, `stabilize` needs no `Typer`: it reports no error and resolves no overload.
        val needsTyper = tree.isErrorTyped || sym.isOverloaded || !sym.isValue && !mode.inQualMode
        analyzer.stabilize(tree, pre, mode, pt, if (needsTyper) fallbackTyper else null)
      }

    /** Analogue of `Typer.typed1` for the tree shapes that synthesised trees, and erased trees, have. */
    protected def typedCore(tree: Tree, mode: Mode, pt: Type): Tree = tree match {
      case tree: Ident =>
        val sym = tree.symbol
        if (sym == NoSymbol) return fallback(tree, pt, "Ident-nosym")
        val pre = if (sym.isTopLevel) sym.owner.thisType else NoPrefix
        stabilize(tree setType pre.memberType(sym), pre, mode, pt)

      case sel @ Select(qual, _) =>
        typedSelect(sel, typedQualifier(qual), mode, pt)

      case Apply(blk @ Block(stats, expr), args) =>
        val ap1 = treeCopy.Apply(tree, expr, args).clearType().setPos(tree.pos.makeTransparent)
        typedCore(treeCopy.Block(blk, stats, ap1).clearType(), mode, pt)

      case Apply(fun, _) =>
        typedApply(tree.asInstanceOf[Apply], typedOperator(fun), mode, pt)

      case ta @ TypeApply(fun, _) =>
        typedTypeApply(treeCopy.TypeApply(ta, typedOperator(fun), ta.args), pt)

      case ReferenceToBoxed(ident) =>
        // Typer.typedReferenceToBoxed
        val ident1 = typed1(ident, mode, pt)
        treeCopy.ReferenceToBoxed(tree, ident1.asInstanceOf[Ident]) setType capturedVariables.capturedVariableType(ident.symbol, erasedTypes = phase.erasedTypes)

      case Literal(value) =>
        tree setType (if (value.tag == UnitTag) UnitTpe else ConstantType(value))

      case tree: This =>
        // Typer.typedThis
        tree.symbol orElse fallbackTyper.qualifyingClass(tree, tree.qual, packageOK = false, immediate = true) match {
          case NoSymbol => tree setType ErrorType // reported by qualifyingClass
          case clazz =>
            tree setSymbol clazz setType clazz.thisType.underlying
            if (analyzer.isStableContext(tree, mode, pt)) tree setType clazz.thisType else tree
        }

      case sup: Super =>
        typedSuper(sup)

      case Typed(expr, tpt0) =>
        // A `TypeTree` comes typed; another type tree is typed here.
        val tpt = if (tpt0.tpe eq null) typed(tpt0) else tpt0
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
        val selector1 = typed(selector)
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
        val lhs1 = typed(lhs)
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
        treeCopy.LabelDef(ldef, ldef.name, params, rhs1) setType restpe

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
        val vparamss1 = vparamss.mapConserve(_.mapConserve(vd => typed(vd).asInstanceOf[ValDef]))
        val savedResult = enclMethodResult
        enclMethodResult = tpt.tpe
        val rhs1 = try atOwner(ddef, meth) {
          if (ddef.name == nme.CONSTRUCTOR && !meth.hasStaticFlag) typed(rhs)
          else typed(rhs, tpt.tpe)
        } finally enclMethodResult = savedResult
        treeCopy.DefDef(ddef, ddef.mods, ddef.name, tparams, vparamss1, tpt, rhs1) setType NoType

      case cdef @ ClassDef(_, _, tparams, impl) =>
        val savedResult = enclMethodResult
        enclMethodResult = NoType
        val impl1 = try atOwner(cdef, cdef.symbol)(typed(impl)).asInstanceOf[Template]
        finally enclMethodResult = savedResult
        treeCopy.ClassDef(cdef, cdef.mods, cdef.name, tparams, impl1) setType NoType

      case templ @ Template(parents, _, body) =>
        val clazz = currentOwner
        val body1 = atOwner(templ, clazz) {
          body mapConserve { stat =>
            if (stat.isDef) typed(stat)
            else atOwner(stat, templ.symbol)(typed(stat))
          }
        }
        treeCopy.Template(templ, parents, noSelfType, body1) setType clazz.tpe_*

      case pdef @ PackageDef(pid, stats) =>
        val pid1 = typedQualifier(pid).asInstanceOf[RefTree]
        val stats1 = atOwner(pdef, pdef.symbol)(stats mapConserve (stat => typed(stat)))
        treeCopy.PackageDef(pdef, pid1, stats1) setType NoType

      case fun @ Function(vparams, body) if !phase.erasedTypes =>
        // Typer.typed1 and Typer.doTypedFunction past typer, for a function with no expected
        // function or SAM type, which would give its body an expected type.
        if (pt ne WildcardType) return fallback(tree, pt, "Function-pt")
        if (fun.symbol == NoSymbol) fun.symbol = enclosingOwner.newAnonymousFunctionValue(fun.pos)
        val vparams1 = vparams.mapConserve(vd => typed(vd).asInstanceOf[ValDef])
        val body1 = atOwner(fun, fun.symbol)(typed(body))
        val resTp = packedType(body1, fun.symbol).resultType.deconst match {
          case ct: ConstantType => ct.widen
          case tp => tp
        }
        val funTp = appliedType(FunctionClass(vparams.length), vparams.map(_.symbol.tpe) :+ resTp)
        treeCopy.Function(fun, vparams1, body1) setType funTp

      case _ =>
        fallback(tree, pt, "core-" + tree.getClass.getSimpleName)
    }

    /** `sel` on its typed qualifier `qual1`: `Typer.typedSelect`. */
    protected final def typedSelect(sel: Select, qual1: Tree, mode: Mode, pt: Type): Tree = {
      val sym = sel.symbol orElse qual1.tpe.member(sel.name)
      if (sym == NoSymbol) return fallback(treeCopy.Select(sel, qual1, sel.name), pt, "Select-nosym")
      // `qual1` is tested first: `alternatives` allocates a list for a non-overloaded symbol.
      if (qual1.symbol != null && qual1.symbol.hasPackageFlag && sym.alternatives.head.owner.isPackageObjectClass)
        // As `Typer.typedSelect`, select a package object's member from the package object.
        return typedSelect(sel, typedQualifier(atPos(qual1.pos.focus)(Select(qual1, nme.PACKAGE))), mode, pt)
      if (phase.erasedTypes && qual1.isInstanceOf[Super]) {
        // After erasure, a selection from `super` is typed against the member's owner, or the accessible class
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
      if (sym.isOverloaded) sel1 else stabilize(sel1, qual1.tpe, mode, pt)
    }

    /** `app` with its typed function `fun1`: types the arguments against the parameter types. */
    protected final def typedApply(app: Apply, fun1: Tree, mode: Mode, pt: Type): Tree = {
      // Typer.normalTypedApply
      val fun2 = {
        val sym = fun1.symbol
        if ((sym ne null) && sym.isMethod && sym.isStable) analyzer.stabilizeFun(fun1, mode, pt)
        else fun1
      }
      def typedArgs = app.args.mapConserve(typed(_))
      fun2.tpe match {
        case _ if treeInfo.isWildcardStarArgList(app.args) =>
          fallback(treeCopy.Apply(app, fun2, app.args), pt, "Apply-star")
        case mt @ MethodType(params, _) if sameLength(params, app.args) =>
          val args1 = map2Conserve(app.args, params)((arg, param) => typed(arg, param.tpe))
          // Before erasure, an argument that does not conform needs the typer's conversions, or
          // its error (a translated match can call an extractor's members with the wrong types).
          if (!phase.erasedTypes && !args1.corresponds(params)((arg, param) => conforms(arg.tpe, param.tpe)))
            return fallback(treeCopy.Apply(app, fun2, args1), pt, "Apply-nonconforming")
          // `resultType(actuals)` ignores the actuals when these hold: don't build their list.
          val restpe = if (mt.isTrivial || phase.erasedTypes) mt.resultType else mt.resultType(args1.map(_.tpe))
          treeCopy.Apply(app, fun2, args1) setType restpe
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
              typedApply(treeCopy.Apply(app, fun3, args1), fun3, mode, pt)
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
          typedApply(treeCopy.Apply(app, fun3, args1), fun3, mode, pt)
        case _ =>
          fallback(treeCopy.Apply(app, fun2, app.args), pt, "Apply-" + fun2.tpe.getClass.getSimpleName)
      }
    }

    /** `gen.mkAttributedCast` without a `Typer`: `gen.mkCast` builds `tree.$asInstanceOf[pt]()`
     *  with the `Select` typed, and the type application and the application get their types from
     *  it, as `Typer` would give them.
     */
    protected final def typedSyntheticCast(tree: Tree, pt: Type): Tree = gen.mkCast(tree, pt) match {
      case app @ Apply(ta @ TypeApply(sel: Select, _), Nil) if sel.tpe ne null =>
        typedApply(app, typedTypeApply(ta, WildcardType), EXPRmode, WildcardType)
      case _ =>
        gen.mkAttributedCast(tree, pt) // another shape than `mkCast` builds after uncurry
    }

    /** `ta` with its typed function: instantiates the polymorphic type. */
    protected final def typedTypeApply(ta: TypeApply, pt: Type): Tree = ta.fun.tpe match {
      case _ if ta.args.exists(_.tpe eq null) =>
        // A type argument other than a `TypeTree` (`preErase` leaves those untyped): the typer types it.
        fallback(ta, pt, "TypeApply-untyped-arg")
      case PolyType(tparams, restpe) if tparams.nonEmpty && sameLength(tparams, ta.args) =>
        ta setType restpe.instantiateTypeParams(tparams, ta.args.map(_.tpe))
      case _ =>
        fallback(ta, pt, "TypeApply-nonpoly")
    }

    private def typedSuper(tree: Super): Tree = {
      val qual1 = typed(tree.qual)
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
      stats mapConserve (stat => typed(stat))

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
          val localTyper = fallbackTyper
          devWarning(pat.pos, s"$phase: typing pattern ${pat.getClass.getSimpleName} with ${localTyper.getClass.getSimpleName}")
          localTyper.typedPattern(beforeFallback(pat), pt)
      }
    }
  }
}
