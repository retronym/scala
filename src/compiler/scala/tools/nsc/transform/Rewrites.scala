package scala.tools.nsc
package transform

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.{Position, SourceFile}
import scala.tools.nsc.Reporting.WarningCategory

abstract class Rewrites extends SubComponent with TypingTransformers {
  import global._

  val phaseName = "rewrites"

  def newPhase(prev: Phase): StdPhase = {
    if (settings.Yrewrites.value.isEmpty) {
      new StdPhase(prev) {
        override def apply(unit: global.CompilationUnit): Unit = ()
      }
    } else
      new RewritePhase(prev)
  }

  private class RewritePhase(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      val patches = ArrayBuffer[Patch]()

      val reparseUnit = new CompilationUnit(unit.source)
      reparseUnit.body = newUnitParser(reparseUnit).parse()
      val treeByRangePos = mutable.HashMap[Position, Tree]()

      val settings = global.settings
      val rws = settings.Yrewrites
      if (rws.contains(rws.domain.breakOutArgs)) {
        val rewriter = new BreakoutTraverser(unit)
        rewriter.transform(unit.body)
        patches ++= rewriter.patches
      }
      if (rws.contains(rws.domain.collectionSeq)) {
        val rewriter = new CollectionSeqTransformer(treeByRangePos, unit)
        rewriter.transform(unit.body)
        patches ++= rewriter.patches
      }
      if (rws.contains(rws.domain.varargsToSeq)) {
        val rewriter = new VarargsToSeq(unit)
        rewriter.transform(unit.body)
        patches ++= rewriter.patches
      }
      if (rws.contains(rws.domain.importCollectionsCompat)) {
        val rewriter = new AddImports(unit)
        rewriter.transform(unit.body)
        patches ++= rewriter.patches
      }
      writePatches(unit.source, patches.toArray)
    }
  }

  private case class Patch(span: Position, replacement: String) {
    def delta: Int = replacement.length - (span.end - span.start)
  }

  private def checkNoOverlap(patches: Array[Patch]): Boolean = {
    var ok = true
    if (patches.nonEmpty)
      patches.reduceLeft { (p1, p2) =>
        if (p1.span.end > p2.span.start) {
          ok = false
          runReporting.warning(NoPosition, s"overlapping patches:\n - $p1\n - $p2", WarningCategory.Other, "")
        }
        p2
      }
    ok
  }

  private def applyPatches(source: SourceFile, patches: Array[Patch]): String = {
    val sourceChars = source.content
    val patchedChars = new Array[Char](sourceChars.length + patches.foldLeft(0)(_ + _.delta))

    @tailrec def loop(pIdx: Int, inIdx: Int, outIdx: Int): Unit = {
      def copy(upTo: Int): Int = {
        val untouched = upTo - inIdx
        System.arraycopy(sourceChars, inIdx, patchedChars, outIdx, untouched)
        outIdx + untouched
      }
      if (pIdx < patches.length) {
        val p = patches(pIdx)
        val outNew = copy(p.span.start)
        p.replacement.copyToArray(patchedChars, outNew)
        loop(pIdx + 1, p.span.end, outNew + p.replacement.length)
      } else {
        val outNew = copy(sourceChars.length)
        assert(outNew == patchedChars.length, s"$outNew != ${patchedChars.length}")
      }
    }
    loop(0, 0, 0)
    new String(patchedChars)
  }

  private def writePatches(source: SourceFile, patches: Array[Patch]): Unit = if (patches.nonEmpty) {
    java.util.Arrays.sort(patches, Ordering.by[Patch, Int](_.span.start))
    if (checkNoOverlap(patches)) {
      val bytes = applyPatches(source, patches).getBytes(settings.encoding.value)
      val out = source.file.output
      out.write(bytes)
      out.close()
    }
  }

  private def isInferredArg(tree: Tree) = tree match {
    case tt: TypeTree => tt.original eq null
    case _ =>
      val pos = tree.pos
      pos.isOffset && tree.forAll(t => {
        val tpos = t.pos
        tpos == NoPosition || tpos.isOffset && tpos.point == pos.point
      })
  }

  // Applied.unapply matches any tree, not just applications
  private object Application {
    def unapply(t: Tree): Option[(Tree, List[Tree], List[List[Tree]])] = t match {
      case _: Apply | _: TypeApply =>
        val applied = treeInfo.dissectApplied(t)
        Some((applied.core, applied.targs, applied.argss))
      case _ => None
    }
  }

  private class RewriteTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    var lastTopLevelContext: analyzer.Context = analyzer.NoContext
    var topLevelImportPos: Position = unit.source.position(0)
    override def transform(tree: Tree): Tree = tree match {
      case pd: PackageDef =>
        topLevelImportPos = pd.pid.pos.focusEnd
        atOwner(tree.symbol) {
          lastTopLevelContext = localTyper.context
        }
        super.transform(tree)
      case imp: Import =>
        localTyper.context = localTyper.context.make(imp)
        val context = localTyper.context
        if (context.enclClass.owner.hasPackageFlag) {
          lastTopLevelContext = context
          topLevelImportPos = tree.pos.focusEnd
        }
        super.transform(imp)
      case tt: TypeTree if tt.original != null =>
        val saved = tt.original.tpe
        tt.original.tpe = tt.tpe
        try transform(tt.original)
        finally tt.original.setType(saved)
      case Block(stats, expr) =>
        val entered = mutable.ListBuffer[Symbol]()
        val saved = localTyper
        def enter(sym: Symbol): Unit = {
          entered += sym
          localTyper.context.scope.enter(sym)
        }
        try {
          val stats1 = stats.mapConserve { stat =>
            stat match {
              case md: MemberDef =>
                val sym = md.symbol
                if (sym != NoSymbol)
                  enter(stat.symbol)
              case imp: Import   =>
                localTyper.context = localTyper.context.make(imp)
              case _             =>
            }
            transform(stat)
          }
          val expr1  = transform(expr)
        } finally {
          localTyper = saved
          entered.foreach(localTyper.context.scope.unlink(_))
        }
        treeCopy.Block(tree, stats1, expr1)
      case dd: DefDef =>
        localTyper.reenterTypeParams(dd.tparams)
        localTyper.reenterValueParams(dd.vparamss)
        try super.transform(dd)
        finally {
          val scope = localTyper.context.scope
          dd.tparams.foreach(tree => scope.unlink(tree.symbol))
          mforeach(dd.vparamss)(tree => scope.unlink(tree.symbol))
        }
      case cd: ClassDef =>
        typer.reenterTypeParams(cd.tparams)
        // TODO: what about constructor params?
        try super.transform(cd)
        finally {
          val scope = localTyper.context.scope
          cd.tparams.foreach(tree => scope.unlink(tree.symbol))
        }
      case _ => super.transform(tree)
    }

    def silentTyped(tree: Tree, mode: Mode): Tree = {
      val typer = localTyper
      val result = typer.silent[Tree](_.typed(tree, mode))
      result match {
        case analyzer.SilentResultValue(tree: Tree) =>
          tree
        case _: analyzer.SilentTypeError =>
          EmptyTree
      }
    }
    def chooseQualifierExpr(options: List[String])(f: Tree => Boolean): String = {
      options.find { qual =>
        val ref = newUnitParser(newCompilationUnit(qual)).parseRule(_.expr())
        val typed = silentTyped(ref, Mode.QUALmode)
        f(typed)
      }.get
    }
  }

  private class TypeRenderer(rewriteTransformer: RewriteTypingTransformer) extends TypeMap {
    override def apply(tp: Type): Type = tp match {
      case SingleType(pre, sym) if tp.prefix.typeSymbol.isOmittablePrefix =>
        adjust(tp, pre, sym, Mode.QUALmode)((pre1, sym1) => SingleType(pre1, sym1))
      case TypeRef(pre, sym, args) =>
        val args1 = args.mapConserve(this)
        adjust(tp, pre, sym, Mode.TAPPmode | Mode.FUNmode)((pre1, sym1) => TypeRef(pre1, sym1, args1))
      case _ =>
        mapOver(tp)
    }

    def adjust(tp: Type, pre: Type, sym: Symbol, mode: Mode)(f: (Type, Symbol) => Type): Type = {
      if (pre.typeSymbol.isOmittablePrefix || global.shorthands.contains(sym.fullName)) {
        val typedTree = rewriteTransformer.silentTyped(Ident(sym.name), mode)
        if (typedTree.symbol == sym || sym.tpeHK =:= typedTree.tpe)
          f(NoPrefix, sym)
        else {
          val dummyOwner = NoSymbol.newClassSymbol(TypeName(pre.typeSymbol.fullName))
          dummyOwner.setInfo(ThisType(dummyOwner))
          val pre1 = pre match {
            case ThisType(_) | SingleType(_, _) => SingleType(NoPrefix, dummyOwner)
            case _ => TypeRef(NoPrefix, dummyOwner, Nil)
          }
          f(pre1, sym.cloneSymbol(NoSymbol))
        }
      } else {
        mapOver(tp)
      }
    }
  }

  // Rewrites

  private object BreakoutTraverser {
    lazy val breakOutSym = {
      import definitions._
      getMemberMethod(rootMirror.getPackageObject("scala.collection"), TermName("breakOut"))
    }
  }

  private class BreakoutTraverser(unit: CompilationUnit) extends RewriteTypingTransformer(unit) {
    import BreakoutTraverser._
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    override def transform(tree: Tree): Tree = tree match {
      case Application(fun, targs, argss) if fun.symbol == breakOutSym =>
        val inferredBreakOut = targs.forall(isInferredArg) && mforall(argss)(isInferredArg)
        val targsString = {
          val renderer = new TypeRenderer(this)
          (TypeTree(definitions.AnyTpe) +: targs.tail).map(targ => renderer.apply(targ.tpe)).mkString("[", ", ", "]")
        }
        if (inferredBreakOut) {
          patches += Patch(Position.offset(tree.pos.source, fun.pos.end), targsString)
        }
        super.transform(fun)
        tree
      case _ =>
        super.transform(tree)
    }
  }

  private class VarargsToSeq(unit: CompilationUnit) extends RewriteTypingTransformer(unit) {
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    val CollectionImmutableSeq = rootMirror.requiredClass[scala.collection.immutable.Seq[_]]
    val CollectionSeq = rootMirror.requiredClass[scala.collection.Seq[_]]
    override def transform(tree: Tree): Tree = tree match {
      case Typed(expr, Ident(tpnme.WILDCARD_STAR))
        if !expr.tpe.typeSymbol.isNonBottomSubClass(CollectionImmutableSeq) && expr.tpe.typeSymbol.isNonBottomSubClass(CollectionSeq) =>
        patches += Patch(expr.pos.focusEnd, ".toSeq")
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }
  }

  /** Rewrites Idents that refer to scala.Seq/IndexedSeq as collection.Seq (or scala.collection.Seq if qualification is needed) */
  private class CollectionSeqTransformer(treeByRangePos: collection.Map[Position, Tree], unit: CompilationUnit) extends RewriteTypingTransformer(unit) {
    case class Rewrite(name: String, typeAlias: Symbol, termAlias: Symbol, cls: Symbol, module: Symbol)
    val ScalaCollectionPackage = rootMirror.getPackage("scala.collection")
    def rewrite(name: String) = Rewrite(name,
      definitions.ScalaPackage.packageObject.info.decl(TypeName(name)),
      definitions.ScalaPackage.packageObject.info.decl(TermName(name)),
      rootMirror.getRequiredClass("scala.collection." + name),
      rootMirror.getRequiredModule("scala.collection." + name))
    val rewrites = List(rewrite("Seq"), rewrite("IndexedSeq"))
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    override def transform(tree: Tree): Tree = {
      tree match {
        case ref: RefTree =>
          for (rewrite <- rewrites) {
            val sym = ref.symbol
            if (sym == rewrite.cls || sym == rewrite.module || sym == rewrite.termAlias || sym == rewrite.typeAlias) {
              treeByRangePos.get(ref.pos) match {
                case Some(Ident(name)) if name.string_==(rewrite.name) =>
                  val qual: String = chooseQualifierExpr(List("collection", "scala.collection", "_root_.scala.collection")) { tree =>
                    tree.tpe.termSymbol == ScalaCollectionPackage
                  }
                  val patchCode = qual + "." + rewrite.name
                  patches += Patch(ref.pos, patchCode)
                case _ =>
              }
            }
          }
        case _ =>
      }
      super.transform(tree)
    }
  }

  /** Add `import scala.collection.compat._` at the top-level */
  private class AddImports(unit: CompilationUnit) extends RewriteTypingTransformer(unit) {
    val ScalaCollectionCompatPackage = rootMirror.getPackage("scala.collection.compat")
    override def transform(tree: Tree): Tree = {
      super.transform(tree)
    }

    def patches = {
      val importAlreadyExists = lastTopLevelContext match {
        case analyzer.NoContext =>
          false
        case ctx =>
          ctx.enclosingContextChain.iterator.map(_.tree).exists {
            case Import(expr, sels) =>
              val sym = expr.tpe.termSymbol
              sym == ScalaCollectionCompatPackage && sels.exists(_.name == nme.WILDCARD)
            case _ => false
          }
      }
      if (importAlreadyExists) Nil
      else Patch(topLevelImportPos, "\nimport scala.collection.compat._\n") :: Nil
    }
  }
}
