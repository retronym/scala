/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import scala.reflect.internal.TypesStats._
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.util.Statistics._
import scala.tools.nsc.classpath.{FlatClassPath, AggregateFlatClassPath, SourceFileEntry}
import scala.tools.nsc.io.AbstractFile
import scala.tools.util.FlatClassPathResolver

/** A subclass of SymbolLoaders that implements browsing behavior.
 *  This class should be used whenever file dependencies and recompile sets
 *  are managed automatically.
 */
abstract class BrowsingLoaders extends GlobalSymbolLoaders {
  val global: Global

  import global._
  import syntaxAnalyzer.{OutlineParser, MalformedInput}

  /** In browse mode, it can happen that an encountered symbol is already
   *  present. For instance, if the source file has a name different from
   *  the classes and objects it contains, the symbol loader will always
   *  reparse the source file. The symbols it encounters might already be loaded
   *  as class files. In this case we return the one which has a sourcefile
   *  (and the other has not), and issue an error if both have sourcefiles.
   */
  override protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    completer.sourcefile match {
      case Some(src) =>
        (if (member.isModule) member.moduleClass else member).associatedFile = src
      case _ =>
    }
    val decls = owner.info.decls
    val existing = decls.lookup(member.name)
    if (existing == NoSymbol) {
      decls enter member
      member
    } else if (existing.sourceFile == null) {
      decls unlink existing
      decls enter member
      member
    } else {
      if (member.sourceFile != null) {
        if (existing.sourceFile != member.sourceFile)
          error(member+"is defined twice,"+
                "\n in "+existing.sourceFile+
                "\n and also in "+member.sourceFile)
      }
      existing
    }
  }

  /** Browse the top-level of given abstract file `src` and enter
   *  eny encountered top-level classes and modules in `root`
   */
  def browseTopLevel(root: Symbol, src: AbstractFile) {
    class BrowserTraverser extends Traverser {
      var packagePrefix = ""
      var entered = 0

      def createPackageSymbol(pos: Position, pid: RefTree): Symbol = {
        analyzer.newNamer(typer.context.make(pid, currentOwner)).createPackageSymbol(pos, pid)
      }

      // TODO can we just get rid of this and rely on currentOwner instead?
      def addPackagePrefix(pkg: Tree): Unit = pkg match {
        case Select(pre, name) =>
          addPackagePrefix(pre)
          packagePrefix += ("." + name)
        case Ident(name) =>
          if (name != nme.EMPTY_PACKAGE_NAME) { // mirrors logic in Namers, see createPackageSymbol
            if (packagePrefix.length != 0) packagePrefix += "."
            packagePrefix += name
          }
        case _ =>
          throw new MalformedInput(pkg.pos.point, "illegal tree node in package prefix: "+pkg)
      }

      private def inPackagePrefix(pkg: Tree)(op: => Unit): Unit = {
        val oldPrefix = packagePrefix
        val oldOwner = currentOwner
        currentOwner = createPackageSymbol(pkg.pos, pkg.asInstanceOf[RefTree]).moduleClass
        addPackagePrefix(pkg)
        op
        currentOwner = oldOwner
        packagePrefix = oldPrefix
      }

      // TODO rename this to clarify what it does...
      def effectiveRoot = currentOwner

      override def traverse(tree: Tree): Unit = tree match {
        case PackageDef(pkg, body) =>
          inPackagePrefix(pkg) { body foreach traverse }

        case ClassDef(_, name, _, _) =>
          val root = currentOwner
          if (root.exists) {
            enterClass(root, name.toString, new SourcefileLoader(src))
            entered += 1
          } else println("prefixes differ: "+packagePrefix+","+root.fullName)
        case ModuleDef(_, name, _) =>
          val root = currentOwner
          if (root.exists) {
            val module = enterModule(root, name.toString, new SourcefileLoader(src))
            entered += 1
            if (name == nme.PACKAGEkw) {
              println("open package module: "+module)
              openPackageModule(module, root)
            }
          } else println("prefixes differ: "+packagePrefix+","+root.fullName)
        case _ =>
      }
    }

//    System.out.println("Browsing "+src)
    val source = getSourceFile(src) // this uses the current encoding
    val body = new OutlineParser(source).parse()
//    System.out.println(body)
    val browser = new BrowserTraverser
    browser.traverse(body)
    if (browser.entered == 0)
      warning("No classes or objects found in "+source+" that go in "+root)
  }

  lazy val sourcePath: FlatClassPath = {
    val sourcePathSettings = new Settings()
    sourcePathSettings.sourcepath.value = settings.sourcepath.value
    new FlatClassPathResolver(sourcePathSettings).result
  }
  private var sourcePathLastModified: Map[AbstractFile, Long] = Map()
  private var enterToplevelsFromSourcePathRunId: RunId = NoRunId

  override def enterToplevelsFromSourcePath(): Unit = {
    if (enterToplevelsFromSourcePathRunId != currentRunId) {
      val start = Statistics.startTimer(BrowsingLoaderStats.sourcePathOutlineParse)
      enterToplevelsFromSourcePathRunId = currentRunId
      val allSources = sourcePath.allSources
      for (source <- sourcePath.allSources) {
        val currentLastModified = source.file.lastModified
        assert(sourcePathLastModified ne null)
        val oldLastModified = sourcePathLastModified.getOrElse(source.file, 0L)
        if (currentLastModified != oldLastModified)
          loaders.enterToplevelsFromSource(NoSymbol, "", source.file)
      }
      sourcePathLastModified = mapFrom(allSources.iterator.map(_.file).toList)(_.file.lastModified)
      Statistics.stopTimer(BrowsingLoaderStats.sourcePathOutlineParse, start)
      println(BrowsingLoaderStats.sourcePathOutlineParse.line)
    }
  }

  /** Enter top-level symbols from a source file
   */
  override def enterToplevelsFromSource(root: Symbol, name: String, src: AbstractFile) {
    try {
      if (root == NoSymbol)
        browseTopLevel(root, src)
      else if (root.isEffectiveRoot || !src.name.endsWith(".scala")) // RootClass or EmptyPackageClass
        super.enterToplevelsFromSource(root, name, src)
      else
        browseTopLevel(root, src)
    } catch {
      case ex: syntaxAnalyzer.MalformedInput =>
        println("[%s] caught malformed input exception at offset %d: %s".format(src, ex.offset, ex.msg))
        super.enterToplevelsFromSource(root, name, src)
    }
  }
}

object BrowsingLoaderStats {
  val sourcePathOutlineParse = newTimer("time spent eagerly outline parsing source files on the source path", "typer")
}