/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This trait ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait EtaExpansion { self: Analyzer =>

  import global._

  def typedEtaExpansion(tree: Tree, pt: Type, mode: Mode, typer: Typer): Tree = {
    import typer.{ context, typed }
    def hasUndets           = context.undetparams.nonEmpty
    debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")
    checkParamsConvertible(tree, tree.tpe, typer)
    val tree0 = etaExpand(context.unit, tree, typer)

    // #2624: need to infer type arguments for eta expansion of a polymorphic method
    // context.undetparams contains clones of meth.typeParams (fresh ones were generated in etaExpand)
    // need to run typer on tree0, since etaExpansion sets the tpe's of its subtrees to null
    // can't type with the expected type, as we can't recreate the setup in (3) without calling typed
    // (note that (3) does not call typed to do the polymorphic type instantiation --
    //  it is called after the tree has been typed with a polymorphic expected result type)
    if (hasUndets)
      typer.instantiate(typed(tree0, mode), mode, pt)
    else
      typed(tree0, mode, pt)
  }

  private def checkParamsConvertible(tree: Tree, tpe0: Type, typer: Typer) {
    def checkParamsConvertible0(tpe: Type) =
      tpe match {
        case MethodType(formals, restpe) =>
          /*
          if (formals.exists(_.typeSymbol == ByNameParamClass) && formals.length != 1)
            error(pos, "methods with `=>`-parameter can be converted to function values only if they take no other parameters")
          if (formals exists (isRepeatedParamType(_)))
            error(pos, "methods with `*`-parameters cannot be converted to function values");
          */
          if (tpe.isDependentMethodType)
            typer.TyperErrorGen.DependentMethodTpeConversionToFunctionError(tree, tpe)
          checkParamsConvertible(tree, restpe, typer)
        case _ =>
      }
    checkParamsConvertible0(tpe0)
  }


  object etaExpansion {
    private def isMatch(vparam: ValDef, arg: Tree) = arg match {
      case Ident(name)  => vparam.name == name
      case _            => false
    }

    def unapply(tree: Tree): Option[(List[ValDef], Tree, List[Tree])] = tree match {
      case Function(vparams, Apply(fn, args)) if (vparams corresponds args)(isMatch) =>
        Some((vparams, fn, args))
      case _ =>
        None
    }
  }

  /** <p>
   *    Expand partial function applications of type `type`.
   *  </p><pre>
   *  p.f(es_1)...(es_n)
   *     ==>  {
   *            <b>private synthetic val</b> eta$f   = p.f   // if p is not stable
   *            ...
   *            <b>private synthetic val</b> eta$e_i = e_i    // if e_i is not stable
   *            ...
   *            (ps_1 => ... => ps_m => eta$f([es_1])...([es_m])(ps_1)...(ps_m))
   *          }</pre>
   *  <p>
   *    tree is already attributed
   *  </p>
   */
  def etaExpand(unit : CompilationUnit, tree: Tree, typer: Typer): Tree = {
    val tpe = tree.tpe
    var cnt = 0 // for NoPosition
    def freshName() = {
      cnt += 1
      unit.freshTermName("eta$" + (cnt - 1) + "$")
    }
    val defs = new ListBuffer[Tree]

    /* Append to `defs` value definitions for all non-stable
     * subexpressions of the function application `tree`.
     */
    def liftoutPrefix(tree: Tree): Tree = {
      def liftout(tree: Tree, byName: Boolean): Tree =
        if (treeInfo.isExprSafeToInline(tree)) tree
        else {
          val vname: Name = freshName()
          // Problem with ticket #2351 here
          defs += atPos(tree.pos) {
            val rhs = if (byName) {
              val res = typer.typed(Function(List(), tree))
              new ChangeOwnerTraverser(typer.context.owner, res.symbol) traverse tree // SI-6274
              res
            } else tree
            ValDef(Modifiers(SYNTHETIC), vname.toTermName, TypeTree(), rhs)
          }
          atPos(tree.pos.focus) {
            if (byName) Apply(Ident(vname), List()) else Ident(vname)
          }
        }
      val tree1 = tree match {
        // a partial application using named arguments has the following form:
        // { val qual$1 = qual
        //   val x$1 = arg1
        //   [...]
        //   val x$n = argn
        //   qual$1.fun(x$1, ..)..(.., x$n) }
        // Eta-expansion has to be performed on `fun`
        case Block(stats, fun) =>
          defs ++= stats
          liftoutPrefix(fun)
        case Apply(fn, args) =>
          val byName: Int => Option[Boolean] = fn.tpe.params.map(p => definitions.isByNameParamType(p.tpe)).lift
          val newArgs = mapWithIndex(args) { (arg, i) =>
            // with repeated params, there might be more or fewer args than params
            liftout(arg, byName(i).getOrElse(false))
          }
          treeCopy.Apply(tree, liftoutPrefix(fn), newArgs).clearType()
        case TypeApply(fn, args) =>
          treeCopy.TypeApply(tree, liftoutPrefix(fn), args).clearType()
        case Select(qual, name) =>
          val name = tree.symbol.name // account for renamed imports, SI-7233
          treeCopy.Select(tree, liftout(qual, byName = false), name).clearType() setSymbol NoSymbol
        case Ident(name) =>
          tree
      }
      if (tree1 ne tree) tree1 setPos tree1.pos.makeTransparent
      tree1
    }

    /* Eta-expand lifted tree. */
    def expand(tree: Tree, tpe: Type): Tree = tpe match {
      case mt @ MethodType(paramSyms, restpe) if !mt.isImplicit =>
        val params: List[(ValDef, Boolean)] = paramSyms.map {
          sym =>
            val origTpe = sym.tpe
            val isRepeated = definitions.isRepeatedParamType(origTpe)
            // SI-4176 Don't leak A* in eta-expanded function types. See t4176b.scala
            val droppedStarTpe = if (settings.etaExpandKeepsStar) origTpe else dropIllegalStarTypes(origTpe)
            val valDef = ValDef(Modifiers(SYNTHETIC | PARAM), sym.name.toTermName, TypeTree(droppedStarTpe), EmptyTree)
            (valDef, isRepeated)
        }
        atPos(tree.pos.makeTransparent) {
          val args = params.map {
            case (valDef, isRepeated) => gen.paramToArg(Ident(valDef.name), isRepeated)
          }
          Function(params.map(_._1), expand(Apply(tree, args), restpe))
        }
      case _ =>
        tree
    }

    val tree1 = liftoutPrefix(tree)
    atPos(tree.pos)(Block(defs.toList, expand(tree1, tpe)))
  }
}
