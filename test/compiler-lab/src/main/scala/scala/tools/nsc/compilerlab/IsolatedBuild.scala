package scala.tools.nsc.compilerlab

import java.net.{URL, URLClassLoader}
import java.nio.file.{FileSystems, Files, Path, Paths}

import scala.jdk.CollectionConverters._

/** What the benchmark drives: this build's `InMemoryCompiler`, or one from another build's class loader. */
trait Compiling {
  def compile(): Unit
  def outputCount: Int
  /** Compile with `SyntheticGlobals(i)` from now on. */
  def useSyntheticGlobal(i: Int, distinctAnalyzers: Boolean): Unit
}

/** Another checkout's build of scala-reflect and the compiler, loaded in its own class loader, so that a fork can
 *  compare builds as it compares flags: a codegen change in the compiler's own code cannot be selected by a flag, and
 *  comparing builds in separate forks needs many forks to see through the fork-to-fork noise.
 *
 *  Each build loads its own reflect and compiler classes and its own copy of the lab's classes (`InMemoryCompiler`,
 *  `Corpus`, ...); everything else (scala-library, scala-asm, the JDK) is the benchmark's own, shared by the builds.
 *  The build's scala-library classes must be byte-identical to the benchmark's (its `library.properties` has a build stamp), so that the shared one is the one it was
 *  compiled with. The builds share the JIT's queue and code cache, and the profiles of library code.
 */
final class IsolatedBuild(val root: Path) {
  import IsolatedBuild._

  private val classes = root.resolve("build/quick/classes")
  for (d <- List("library", "reflect", "compiler") if !Files.isDirectory(classes.resolve(d)))
    sys.error(s"$root: no build/quick/classes/$d; build it first")
  checkSameLibrary(classes.resolve("library"))

  /** The lab's classes as compiled against this build, when it has them: they are binary-coupled to the compiler
   *  (`SyntheticGlobals` subclasses `Analyzer`, which a build may have changed from a trait to a class); else the
   *  benchmark's copy.
   */
  private val lab = {
    val own = root.resolve("test/compiler-lab/target/classes")
    if (Files.isDirectory(own.resolve("scala/tools/nsc/compilerlab"))) own else labClasses
  }

  private val loader = new URLClassLoader(s"build:$root",
    Array(classes.resolve("reflect"), classes.resolve("compiler"), lab).map(_.toUri.toURL), Shared)

  for (c <- List("scala.tools.nsc.Global", "scala.reflect.internal.SymbolTable")) {
    val from = location(loader.loadClass(c))
    if (!from.startsWith(classes)) sys.error(s"$c loaded from $from, not $classes")
  }
  if (loader.loadClass("scala.Option") ne classOf[Option[_]]) sys.error("scala-library is not shared")

  def newCompiler(corpus: String, options: List[String], resident: Boolean): Compiling = {
    val corpusModule = loader.loadClass("scala.tools.nsc.compilerlab.Corpus$")
    val module = corpusModule.getField("MODULE$").get(null)
    val loaded = corpusModule.getMethod("load", classOf[String]).invoke(module, corpus)
    val cls = loader.loadClass("scala.tools.nsc.compilerlab.InMemoryCompiler")
    val target = cls.getConstructors.head.newInstance(loaded, options, Boolean.box(resident))
    val compileMethod = cls.getMethod("compile")
    val outputCountMethod = cls.getMethod("outputCount")
    val useSyntheticGlobalMethod = cls.getMethod("useSyntheticGlobal", Integer.TYPE, java.lang.Boolean.TYPE)
    new Compiling {
      def compile(): Unit = compileMethod.invoke(target)
      def outputCount: Int = outputCountMethod.invoke(target).asInstanceOf[Int]
      def useSyntheticGlobal(i: Int, distinctAnalyzers: Boolean): Unit =
        useSyntheticGlobalMethod.invoke(target, Int.box(i), Boolean.box(distinctAnalyzers))
      override def toString = s"$cls in $loader"
    }
  }
}

object IsolatedBuild {
  private def location(c: Class[_]): Path = Paths.get(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  private val app = getClass.getClassLoader
  private val libraryClasses = location(classOf[scala.Option[_]])
  private val labClasses = location(classOf[InMemoryCompiler])
  /** Classes that each build loads for itself: those that the benchmark loads from these locations. */
  private val perBuild: List[String] =
    List(location(classOf[scala.reflect.internal.SymbolTable]), location(classOf[scala.tools.nsc.Global]), labClasses)
      .flatMap { p => val u = p.toUri.toURL.toString; List(u, s"jar:$u!/") } // a directory, or a jar (sbt's bg-jobs copies)

  /** The benchmark's own classes, but not those that each build loads for itself. */
  private object Shared extends ClassLoader("compiler-lab-shared", ClassLoader.getPlatformClassLoader) {
    private def shared(resource: String): URL = {
      val url = app.getResource(resource)
      if (url == null || perBuild.exists(url.toString.startsWith)) null else url
    }
    override def loadClass(name: String, resolve: Boolean): Class[_] =
      if (shared(name.replace('.', '/') + ".class") == null) throw new ClassNotFoundException(name)
      else app.loadClass(name)
    override def getResource(name: String): URL = shared(name)
    override def getResources(name: String): java.util.Enumeration[URL] =
      java.util.Collections.enumeration(Option(shared(name)).toList.asJava)
  }

  /** The checkout this lab was built in: relative build roots resolve against it. */
  lazy val checkout: Path =
    Iterator.iterate(labClasses)(_.getParent).takeWhile(_ != null).find(d => Files.exists(d.resolve("build.sbt")))
      .getOrElse(sys.error(s"no build.sbt above $labClasses"))

  def apply(root: String): IsolatedBuild = new IsolatedBuild(checkout.resolve(root).normalize)

  private def checkSameLibrary(library: Path): Unit = {
    def files(d: Path) = {
      val s = Files.walk(d)
      try s.iterator.asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class")).map(d.relativize(_).toString).toSet finally s.close()
    }
    val myRoot = if (Files.isDirectory(libraryClasses)) libraryClasses else FileSystems.newFileSystem(libraryClasses).getPath("/")
    val (mine, theirs) = (files(myRoot), files(library))
    val differing = (mine union theirs).toList.sorted.filter { f =>
      !(mine(f) && theirs(f)) || !java.util.Arrays.equals(Files.readAllBytes(myRoot.resolve(f)), Files.readAllBytes(library.resolve(f)))
    }
    if (differing.nonEmpty)
      sys.error(s"$library differs from the benchmark's scala-library ($libraryClasses) in ${differing.size} files, e.g. ${differing.take(5).mkString(", ")}")
  }
}
