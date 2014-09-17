/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2013, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

package scala
package tools.scalap

import java.io.{ PrintStream, OutputStreamWriter, ByteArrayOutputStream }
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
      }
      val global = new CustomGlobal
      new global.Run()
      global.exitingTyper {
        val sym = global.rootMirror.getClassIfDefined(encName)
        global.definitions.fullyInitializeSymbol(sym)
        def symbolToTree(sym: global.Symbol): global.Tree = sym match {
          case x if x.isMethod => global.DefDef(sym, if (x.isDeferred) global.EmptyTree else global.gen.mkZero(global.definitions.NothingTpe))
          case x if x.isValue => global.ValDef(sym)
          case x if x.isAbstractType => global.TypeDef(sym)
          case x if x.isClass => global.ClassDef(sym, sym.info.decls.sorted.map(symbolToTree))
          case x if x.isModule => global.ModuleDef(sym, global.Template(sym, sym.info.decls.sorted.map(symbolToTree)))
          case _ => global.EmptyTree
        }
        val tree = symbolToTree(sym)
        println(global.showCode(tree))
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
