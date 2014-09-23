/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2013, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

package scala
package tools.scalap

import java.io.{PrintWriter, PrintStream, OutputStreamWriter, ByteArrayOutputStream}
import scala.reflect.NameTransformer
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ ClassPath, JavaClassPath }
import scala.tools.util.PathResolver
import ClassPath.DefaultJavaContext
import scala.tools.nsc.io.AbstractFile

/**The main object used to execute scalap on the command-line.
 *
 * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
 */
class Main {
  val versionMsg = "Scala classfile decoder %s -- %s\n".format(Properties.versionString, Properties.copyrightString)

  /**Verbose program run?
   */
  var verbose = false
  var printPrivates = false

  /** Executes scalap with the given arguments and classpath for the
   *  class denoted by `classname`.
   */
  def process(args: Arguments, path: ClassPath[AbstractFile])(classname: String): Unit = {
    // find the classfile
    val encName = classname match {
      case "scala.AnyRef" => "java.lang.Object"
      case _ =>
        // we have to encode every fragment of a name separately, otherwise the NameTransformer
        // will encode using unicode escaping dot separators as well
        // we can afford allocations because this is not a performance critical code
        classname.split('.').map(NameTransformer.encode).mkString(".")
    }
    val cls = path.findClass(encName)
    if (cls.isDefined && cls.get.binary.isDefined) {
      val cfile = cls.get.binary.get
      if (verbose) {
        Console.println(Console.BOLD + "FILENAME" + Console.RESET + " = " + cfile.path)
      }
      val settings = new Settings(msg => throw new RuntimeException(msg))
      class CustomGlobal extends scala.tools.nsc.Global(settings) {
        override lazy val platform: ThisPlatform = new JavaPlatform {
          val global: CustomGlobal.this.type = CustomGlobal.this

          override def classPath: PlatformClassPath = path

        }
        case object CompiledCode extends TermTree {
          override def isTerm = true
        }

        override protected def xtraverse(traverser: Traverser, tree: Tree): Unit = tree match {
          case CompiledCode =>
          case _ => super.xtraverse(traverser, tree)
        }

        override protected def xtransform(transformer: TransformerApi, tree: Tree): Tree = tree match {
          case CompiledCode => CompiledCode
          case _ => super.xtransform(transformer, tree)
        }

        override def xprintTree(treePrinter: TreePrinterInternal, tree: Tree): Unit = tree match {
          case CompiledCode => treePrinter.print("{ /* compiled code */ }")
          case _ => super.xprintTree(treePrinter, tree)

        }

        override def newCodePrinter(writer: PrintWriter, tree: Tree, printRootPkg: Boolean): TreePrinterInternal = new CodePrinter(writer, printRootPkg) {
          override def processTreePrinting(tree: Tree): Unit = {
            super.processTreePrinting(tree)
            tree.attachments.get[CommentTreeAttachment] match {
              case Some(attach) => print(s"/* ${attach.payload} */")
              case _ =>
            }
          }
        }
      }
      val global = new CustomGlobal
      import global._
      new Run()
      exitingTyper {
        val sym = {
          val clsSym = rootMirror.getClassIfDefined(encName)
          if (clsSym.exists) clsSym else rootMirror.getModuleIfDefined(encName)
        }
        definitions.fullyInitializeSymbol(sym)
        def modifyModifiers(tree: MemberDef)(f: Modifiers => Modifiers): Tree = tree match {
          case PackageDef(pid, stats) => tree
          case TypeDef(mods, name, tparams, rhs) => treeCopy.TypeDef(tree, f(mods), name, tparams, rhs)
          case ClassDef(mods, name, tparams, impl) => treeCopy.ClassDef(tree, f(mods), name, tparams, impl)
          case ModuleDef(mods, name, impl) => treeCopy.ModuleDef(tree, f(mods), name, impl)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) => treeCopy.DefDef(tree, f(mods), name, tparams, vparamss, tpt, rhs)
          case ValDef(mods, name, tpt, rhs) => treeCopy.ValDef(tree, f(mods), name, tpt, rhs)
          case _ => tree
        }
        def annotationToTree(info: AnnotationInfo): Tree = {
          New(info.atp, Nil) // TODO args
        }
        def annotate(tree: Tree) = tree match {
          case ann: Annotated => ann
          case md: MemberDef if tree.symbol != null =>
            val annots = tree.symbol.annotations map annotationToTree
            def modMods(mods: Modifiers): Modifiers = {
              val mods1 = mods.copy(annotations = annots)
              val mods2 = mods1.copy(privateWithin = tree.symbol.privateWithin match {
                case NoSymbol => tpnme.EMPTY
                case sym => sym.name
              })
              mods2
            }
            modifyModifiers(md)(modMods)
          case _ => tree
        }
        def symbolToTree(sym: Symbol): Tree = annotate(sym match {
          case x if x.isMethod =>
            val rhs =
              if (x.isDeferred) EmptyTree
              else if (x.isPrimaryConstructor) Block(gen.mkSyntheticUnit(), CompiledCode)
              else CompiledCode
              DefDef(sym, rhs)
          case x if x.isAbstractType || x.isTypeParameter => TypeDef(sym)
          case x if x.isClass =>
            ClassDef(sym, sym.info.decls.sorted.map(symbolToTree))
          case x if x.isModule => ModuleDef(sym, Template(sym, sym.info.decls.sorted.map(symbolToTree)))
          case x if x.isValue =>
            ValDef(sym)
          case _ => EmptyTree
        })
        val tree = symbolToTree(sym)

        object TypeExpander extends Transformer {
          private def transformType(tp: Type) = transform(TypeTree(tp))
          override def transform(tree: Tree): Tree = tree match {
            case TypeTree() =>
              tree.tpe match {
                case ExistentialType(quantified, underlying) => ExistentialTypeTree(transformType(underlying), transformTrees(quantified map symbolToTree).asInstanceOf[List[MemberDef]])
                case AnnotatedType(annotations, underlying) => (transformType(underlying) /: annotations)((accum, annot) => Annotated(annotationToTree(annot), accum))
                case tp: CompoundType => CompoundTypeTree(Template(tp.parents map transformType, emptyValDef /*TODO*/ , transformTrees(tp.decls.sorted.map(symbolToTree))))
                case TypeRef(pre, sym, args) =>
                  val typefun = pre match {
                    case NoPrefix =>
                      Ident(sym)
                    case _ =>
                      def qualifiedRef = {
                        val preTree = transformType(pre)
                        preTree match {
                          case SingletonTypeTree(ref) =>
                            Select(ref, sym.name.toTypeName).setSymbol(sym)
                          case _ =>
                            SelectFromTypeTree(preTree, sym.name.toTypeName).setSymbol(sym)
                        }
                      }
                      pre match {
                        case ThisType(thissym) if thissym.info.decl(sym.name) != sym =>
                          val superSym = thissym.parentSymbols.reverseIterator.find(_.info.member(sym.name) == sym).getOrElse(sym.owner /*TODO*/)
                          Select(Super(This(pre.typeSymbol), superSym.name.toTypeName), sym.name.toTypeName)
                        case _ =>
                          qualifiedRef
                      }
                  }
                  gen.mkAppliedTypeTree(typefun, args map transformType)
                case ThisType(sym) => SingletonTypeTree(This(sym))
                case SingleType(pre, sym) => SingletonTypeTree(Select(transformType(pre), sym.name).setSymbol(sym))
                case ConstantType(const) =>
                  transformType(const.tpe).updateAttachment(new CommentTreeAttachment(const.toString))
              }
            case TypeBoundsTree(lo, hi) =>
              val loTree = if (lo.tpe == definitions.NothingTpe) EmptyTree else lo
              val hiTree = if (hi.tpe == definitions.AnyTpe) EmptyTree else hi
              val t = TypeBoundsTree(loTree, hiTree)
              println((lo, hi, t))
              t
            case _ => super.transform(tree)
          }
        }
        val tree2 = TypeExpander.transform(tree)

        println(showCode(tree2, printRootPkg = true))
      }
    }
    else
      Console.println("class/object " + classname + " not found.")
  }

  object EmptyClasspath extends ClassPath[AbstractFile] {
    /**
     * The short name of the package (without prefix)
     */
    def name              = ""
    def asURLs            = Nil
    def asClasspathString = ""

    val context     = DefaultJavaContext
    val classes     = IndexedSeq()
    val packages    = IndexedSeq()
    val sourcepaths = IndexedSeq()
  }
}

object Main extends Main {
  /** Prints usage information for scalap. */
  def usage() {
    Console println """
      |Usage: scalap {<option>} <name>
      |where <name> is fully-qualified class name or <package_name>.package for package objects
      |and <option> is
      |  -private           print private definitions
      |  -verbose           print out additional information
      |  -version           print out the version number of scalap
      |  -help              display this usage message
      |  -classpath <path>  specify where to find user class files
      |  -cp <path>         specify where to find user class files
    """.stripMargin.trim
  }

  def main(args: Array[String]) {
    // print usage information if there is no command-line argument
    if (args.isEmpty)
      return usage()

    val arguments = Arguments.Parser('-')
            .withOption("-private")
            .withOption("-verbose")
            .withOption("-version")
            .withOption("-help")
            .withOptionalArg("-classpath")
            .withOptionalArg("-cp")
            .parse(args);

    if (arguments contains "-version")
      Console.println(versionMsg)
    if (arguments contains "-help")
      usage()

    verbose       = arguments contains "-verbose"
    printPrivates = arguments contains "-private"
    // construct a custom class path
    val cparg = List("-classpath", "-cp") map (arguments getArgument _) reduceLeft (_ orElse _)
    val path = cparg match {
      case Some(cp) => new JavaClassPath(DefaultJavaContext.classesInExpandedPath(cp), DefaultJavaContext)
      case _        => PathResolver.fromPathString(".") // include '.' in the default classpath SI-6669
    }
    // print the classpath if output is verbose
    if (verbose)
      Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)

    // process all given classes
    arguments.getOthers foreach process(arguments, path)
  }
}

private class CommentTreeAttachment(val payload: String)
