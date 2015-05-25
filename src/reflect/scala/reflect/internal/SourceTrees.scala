/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal

import util._

/**
 * Utilities to recover pre-typer shaped trees from post-typer trees or symbols.
 */
trait SourceTrees {
  val symtab: SymbolTable
  import symtab._

  protected def rhsTree: Tree = EmptyTree

  import definitions._
  private def modifyModifiers(tree: MemberDef)(f: Modifiers => Modifiers): Tree = tree match {
    case PackageDef(pid, stats) => tree
    case TypeDef(mods, name, tparams, rhs) => treeCopy.TypeDef(tree, f(mods), name, tparams, rhs)
    case ClassDef(mods, name, tparams, impl) => treeCopy.ClassDef(tree, f(mods), name, tparams, impl)
    case ModuleDef(mods, name, impl) => treeCopy.ModuleDef(tree, f(mods), name, impl)
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) => treeCopy.DefDef(tree, f(mods), name, tparams, vparamss, tpt, rhs)
    case ValDef(mods, name, tpt, rhs) => treeCopy.ValDef(tree, f(mods), name, tpt, rhs)
    case _ => tree
  }

  private def annotate(tree: Tree): Tree = tree match {
    case ann: Annotated => ann
    case md: MemberDef if tree.symbol != null =>
      val annots = tree.symbol.annotations map annotationToTree
      def modMods(mods: Modifiers): Modifiers = {
        val mods1 = mods.copy(annotations = annots)
        val mods2 = mods1.copy(privateWithin = tree.symbol.privateWithin match {
          case NoSymbol => tpnme.EMPTY
          case sym => sym.name
        })
        if (tree.symbol.owner.isPrimaryConstructor) {
          val cls = tree.symbol.owner.owner
          val paramAccessors = cls.info.decls.filter(sym => sym.isMethod && sym.isParamAccessor).toList
          val constrParams: List[Symbol] = tree.symbol.owner.paramss.flatten
          val paramAccessor = paramAccessors.find(acc => {
            constrParameterNamed(constrParams, acc.unexpandedName.getterName) == tree.symbol
          }).getOrElse(NoSymbol)
          if (paramAccessor != NoSymbol && paramAccessor.isOverride)
            mods2 | Flag.OVERRIDE
          else
            mods2
        } else mods2
      }
      modifyModifiers(md)(modMods)
    case _ => tree
  }

  def sourceDefTree(sym: Symbol): Tree = {
    definitions.fullyInitializeSymbol(sym)
    exitingPhase(findPhaseWithName("typer")) {
      sourceTree.transform(symbolToTree(sym))
    }
  }

  def symbolToTree(sym: Symbol): Tree = annotate(sym match {
    case x if x.isMethod =>
      val rhs =
        if (x.isDeferred) EmptyTree
        else if (x.isPrimaryConstructor) Block(gen.mkSyntheticUnit(), rhsTree)
        else if (x.isConstructor)
          Block(Apply(This(tpnme.EMPTY), Nil), rhsTree)
        else rhsTree
      val tree = DefDef(sym, rhs)
      copyDefDef(tree)(vparamss = mmap(sym.paramss)(symbolToTree).asInstanceOf[List[List[ValDef]]], tpt = if (sym.isAuxiliaryConstructor) EmptyTree else null)
    case x if x.isAliasType => TypeDef(sym, TypeTree(sym.tpeHK.dealias))
    case x if x.isAbstractType || x.isTypeParameter => TypeDef(sym)
    case x if x.isClass =>
      ClassDef(sym, sym.info.decls.sorted.map(symbolToTree))
    case x if x.isModule => ModuleDef(sym, Template(sym, sym.info.decls.sorted.map(symbolToTree)))
    case x if x.isValue =>
      val result = ValDef(sym, rhsTree)
      if (result.mods.isLazy && result.mods.isMutable)
        copyValDef(result)(mods = result.mods &~ Flags.MUTABLE) // TODO fix this in ReificationSupport ?
      else result
    case _ => EmptyTree
  })

  object sourceTree extends Transformer {
    import rootMirror.RootClass
    private def transformType(tp: Type) = transform(TypeTree(tp))
    def fullyQualified(sym: Symbol): Tree = {
      val ownersReverse: List[Symbol] = sym.ownerChain.takeWhile(_ != RootClass).reverse
      val root: Tree = gen.mkAttributedIdent(RootClass.sourceModule)
      ownersReverse.foldLeft(root)((accum, sym) => gen.mkAttributedSelect(accum, sym.sourceModule.orElse(sym)))
    }

    override def transform(tree: Tree): Tree = tree match {
      case TypeTree() =>
        tree.tpe match {
          case ExistentialType(quantified, underlying) =>
            val underlying1: Tree = transformType(underlying)
            val whereClauses: List[MemberDef] = transformTrees(quantified map symbolToTree).asInstanceOf[List[MemberDef]]
            treeCopy.ExistentialTypeTree(tree, underlying1, whereClauses)
          case AnnotatedType(annotations, underlying) => 
            (transformType(underlying) /: annotations)((accum, annot) => Annotated(annotationToTree(annot), accum))
          case tp: CompoundType =>
            val decls = tp.decls.sorted.map(symbolToTree).map {
              case md: MemberDef =>
                modifyModifiers(md)(_ &~ Flag.OVERRIDE & Flag.DEFERRED) match {
                  case vd: ValDef => copyValDef(vd)(rhs = EmptyTree)
                  case dd: DefDef => copyDefDef(dd)(rhs = EmptyTree)
                  case t => t
                }
              case t => t
            }
            CompoundTypeTree(Template(tp.parents map transformType, emptyValDef /*TODO*/ , transformTrees(decls)))
          case TypeRef(_, sym, args) if sym.isExistential || sym.isTypeParameter =>
            gen.mkAppliedTypeTree(Ident(sym), args map transformType)
          case TypeRef(pre, sym, args) =>
            val typefun = pre match {
              case NoPrefix =>
                fullyQualified(sym)
              case _ =>
                def qualifiedRef = {
                  val preTree = transformType(pre)
                  preTree match {
                    case SingletonTypeTree(ref) if sym.isType =>
                      Select(ref, sym.name.toTypeName).setSymbol(sym)
                    case _ =>
                      SelectFromTypeTree(preTree, sym.name.toTypeName).setSymbol(sym)
                  }
                }
                pre match {
                  case ThisType(thissym) if !thissym.isPackageClass && sym.isType && thissym.info.decl(sym.name) != sym =>
                    val superSym = thissym.parentSymbols.reverseIterator.find(p => p.exists && p.info.member(sym.name) == sym).getOrElse(sym.owner /*TODO*/)
                    Select(Super(This(pre.typeSymbol), superSym.name.toTypeName), sym.name.toTypeName)
                  case SingleType(_, thissym) if thissym.isPackageClass =>
                    Select(fullyQualified(thissym), sym.name).setSymbol(sym)
                  case SingleType(_, _) if pre.typeSymbol.isPackageObjectClass =>
                    Select(Select(fullyQualified(pre.typeSymbol.owner), nme.PACKAGE), sym).setSymbol(sym)
                  case ThisType(thissym) if thissym.isStaticOwner =>
                    Select(fullyQualified(thissym), sym.name).setSymbol(sym)
                  case SingleType(pre, thissym)  =>
                    Select(transformType(typeRef(pre, thissym, Nil)), sym.name).setSymbol(sym)
                  case ThisType(thissym)  =>
                    Select(This(thissym), sym.name).setSymbol(sym)
                  case _ =>
                    qualifiedRef
                }
            }
            gen.mkAppliedTypeTree(typefun, args map transformType)
          case ThisType(sym) =>
            SingletonTypeTree(This(sym))
          case SingleType(pre1, sym) if pre1.typeSymbol.isPackageClass && sym.isTerm =>
            SingletonTypeTree(Select(fullyQualified(pre1.typeSymbol), sym.name).setSymbol(sym))
          case SingleType(ThisType(thissym), sym) if sym.isTerm =>
            SingletonTypeTree(Select(This(thissym), sym.name).setSymbol(sym))
          case SingleType(pre, sym) =>
            SingletonTypeTree(Select(transformType(pre), sym.name).setSymbol(sym))
          case ConstantType(const) =>
            transformType(const.tpe) //.updateAttachment(new CommentTreeAttachment(const.toString))
        }
      case TypeBoundsTree(lo, hi) =>
        val loTree = if (lo.tpe == NothingTpe) EmptyTree else lo
        val hiTree = if (hi.tpe == AnyTpe) EmptyTree else hi
        TypeBoundsTree(loTree, hiTree)
      case _ => super.transform(tree)
    }
  }
}
