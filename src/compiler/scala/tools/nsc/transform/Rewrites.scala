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
      reparseUnit.body.foreach {
        tree =>
          if (tree.pos.isRange && !tree.pos.isTransparent)
            treeByRangePos(tree.pos) = tree
      }

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

    override def transform(tree: Tree): Tree = tree match {
      case tt: TypeTree if tt.original != null =>
        val saved = tt.original.tpe
        tt.original.tpe = tt.tpe
        try transform(tt.original)
        finally tt.original.setType(saved)
      case Block(stats, expr) =>
        val stats1 = stats.mapConserve { stat =>
          stat match {
            case md: MemberDef =>
              val sym = md.symbol
              if (sym != NoSymbol)
                localTyper.context.scope.enter(stat.symbol)
            case imp: Import =>
              localTyper.context = localTyper.context.make(imp)
            case _ =>
          }
          transform(stat)
        }
        val expr1 = transform(expr)
        treeCopy.Block(tree, stats1, expr1)
      case dd: DefDef =>
        typer.reenterTypeParams(dd.tparams)
        typer.reenterValueParams(dd.vparamss)
        super.transform(dd)
      case cd: ClassDef =>
        typer.reenterTypeParams(cd.tparams)
        // TODO: what about constructor params?
        super.transform(cd)
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
          targs.map(targ => renderer.apply(targ.tpe)).mkString("[", ", ", "]")
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
  private class TypeRenderer(rewriteTransformer: RewriteTypingTransformer) extends TypeMap {
    override def apply(tpe: Type): Type = tpe match {
      case SingleType(pre, sym) if tpe.prefix.typeSymbol.isOmittablePrefix =>
        if (pre.typeSymbol.isOmittablePrefix) {
          val typedTree = rewriteTransformer.silentTyped(Ident(sym.name), Mode.QUALmode)
          if (typedTree.symbol == sym || sym.tpeHK =:= typedTree.tpe)
            SingleType(NoPrefix, sym)
          else {
            val dummyOwner = NoSymbol.newClassSymbol(TypeName(pre.typeSymbol.fullName))
            dummyOwner.setInfo(ThisType(dummyOwner))
            SingleType(TypeRef(NoPrefix, dummyOwner, Nil), sym.cloneSymbol(NoSymbol))
          }
        } else {
          mapOver(tpe)
        }
      case TypeRef(pre, sym, args) =>
        val args1 = args.mapConserve(this)
        if (pre.typeSymbol.isOmittablePrefix || global.shorthands.contains(sym.fullName)) {
          if (sym.name.string_==("List"))
            getClass
          val typedTree = rewriteTransformer.silentTyped(Ident(sym.name), Mode.TAPPmode | Mode.FUNmode)
          if (typedTree.symbol == sym || sym.tpeHK =:= typedTree.tpe)
            TypeRef(NoPrefix, sym, args1)
          else {
            val dummyOwner = NoSymbol.newClassSymbol(TypeName(pre.typeSymbol.fullName))
            dummyOwner.setInfo(ThisType(dummyOwner))
            TypeRef(SingleType(NoPrefix, dummyOwner), sym.cloneSymbol(NoSymbol), args1)
          }
        } else {
          mapOver(tpe)
        }
      case _ =>
        mapOver(tpe)
    }
  }
}
