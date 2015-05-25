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
import scala.reflect.internal.{SymbolTable, SourceTrees, Flags}
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

    val settings = new Settings(msg => throw new RuntimeException(msg))
//    settings.usejavacp.value = true
    class CustomGlobal extends scala.tools.nsc.Global(settings) {
      override lazy val platform: ThisPlatform = new JavaPlatform {
        val global: CustomGlobal.this.type = CustomGlobal.this

        override def classPath: PlatformClassPath = path
      }
      val CompiledCode = Ident(nme.EMPTY)

      override def newCodePrinter(writer: PrintWriter, tree: Tree, printRootPkg: Boolean): TreePrinterInternal = new CodePrinter(writer, printRootPkg) {
        override def processTreePrinting(tree: Tree): Unit = {
          if (tree eq CompiledCode)
            print("{ throw null; /* compiled code */ }")
          else super.processTreePrinting(tree)
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

    val sourceTrees = new SourceTrees {
      override val symtab: global.type = global
      override protected def rhsTree = CompiledCode
    }
    import sourceTrees.sourceDefTree
    val clsSym = rootMirror.getClassIfDefined(encName)
    val moduleSym = rootMirror.getModuleIfDefined(encName).orElse(clsSym.companionModule /* getClassIfDefined might have followed a type alias */)
    val sym = if (clsSym.exists) clsSym else if (moduleSym.exists) moduleSym else NoSymbol
    if (sym == NoSymbol) {
      println(s"No class/object named $encName found")
    } else {
      val owner = sym.owner
      val stats = (if (clsSym.exists) List(sourceDefTree(clsSym)) else Nil) ++ (if (moduleSym.exists) List(sourceDefTree(moduleSym)) else Nil)
      val tree = PackageDef(Ident(owner.fullName), stats)
      println("// decompiled from " + sym.associatedFile)
      println(showCode(tree, printRootPkg = true))
    }
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
//    // construct a custom class path
//    val cparg = List("-classpath", "-cp") map (arguments getArgument _) reduceLeft (_ orElse _)
//    val path: JavaClassPath = cparg match {
//      case Some(cp) => new JavaClassPath(DefaultJavaContext.classesInExpandedPath(cp), DefaultJavaContext)
//      case _        => PathResolver.fromPathString(".") // include '.' in the default classpath SI-6669
//    }

    val settings = new Settings()
    val rest = settings.processArguments(args.toList, processAll = false)._2
    val path: JavaClassPath = new PathResolver(settings).result

    // print the classpath if output is verbose
    if (verbose)
      Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)

    // process all given classes
    arguments.getOthers foreach process(arguments, path)
  }
}

private class CommentTreeAttachment(val payload: String)
