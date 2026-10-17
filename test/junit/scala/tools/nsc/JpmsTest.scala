package scala.tools.nsc

import java.io.{ByteArrayOutputStream, File}
import java.nio.file._
import java.util.jar.Attributes.Name
import java.util.jar.Manifest

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.{After, Assume, Before, Test}

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.SourceFile
import scala.tools.asm.{ClassWriter, Opcodes}
import scala.tools.nsc.reporters.StoreReporter

/** Runs the JPMS tests against both class path implementations. */
class JpmsNativeClassPathTest extends JpmsTest("native")
class JpmsJavacClassPathTest extends JpmsTest("javac")

abstract class JpmsTest(classPathImpl: String) {
  private var fileFactory: FileFactory = _

  @Before def before(): Unit = {
    fileFactory = new FileFactory()
  }
  @After def after(): Unit = if (fileFactory != null) fileFactory.close()

  // The scala library classes in the sbt / partest test run are an exploded directory rather than a JAR.
  // Model them as the automatic module `scala.library` by putting a dummy JAR with the Automatic-Module-Name
  // on the module path and patching the real classes into it.
  private lazy val libraryClasses: String =
    sys.props("java.class.path").split(File.pathSeparator).find(e => Paths.get(e).getFileName.toString == "library").get

  private lazy val scalaLibraryModule: Path = {
    val dir = fileFactory.tempDir()
    createAutomaticModuleJar(dir.resolve("scala-library.jar"), "scala.library")
  }

  private def newGlobal(outputDir: Path, modulePath: List[Path] = Nil, extraArgs: List[String] = Nil): Global = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    val fullModulePath = (scalaLibraryModule :: modulePath).mkString(File.pathSeparator)
    val args = List("-Yjpms", s"-Yjpms-classpath:$classPathImpl", "-d", outputDir.toString, "--module-path", fullModulePath, s"--patch-module:scala.library=$libraryClasses") ++ extraArgs
    val (ok, residual) = settings.processArguments(args, processAll = true)
    assertTrue(args.toString, ok && residual.isEmpty)
    new Global(settings, new StoreReporter(settings))
  }

  private def compileWith(global: Global, sources: List[SourceFile]): List[String] = {
    val reporter = global.reporter.asInstanceOf[StoreReporter]
    reporter.reset()
    new global.Run().compileSources(sources)
    reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg)
  }

  private def compile(sources: List[SourceFile], outputDir: Path, modulePath: List[Path] = Nil, extraArgs: List[String] = Nil): List[String] =
    compileWith(newGlobal(outputDir, modulePath, extraArgs), sources)

  private def assertNoErrors(errors: List[String]): Unit = assertEquals("", errors.mkString("\n"))
  private def assertSingleError(errors: List[String], expected: String): Unit = errors match {
    case msg :: Nil => assertTrue(msg, msg.contains(expected))
    case msgs       => throw new AssertionError(msgs.mkString("expected a single error, got:\n", "\n", ""))
  }

  // An incubator module is observable but not resolved by default for code in the unnamed module (JEP 11).
  private val usesIncubator =
    """package p1 {
      |  package impl {
      |    class C {
      |      def foo = jdk.incubator.vector.IntVector.SPECIES_PREFERRED
      |    }
      |  }
      |  package api {
      |    class P1Api
      |  }
      |}
      |""".stripMargin

  @Test def unnamedModuleCannotReadUnresolvedModule(): Unit = {
    val errors = compile(fileFactory(usesIncubator) :: Nil, fileFactory.tempDir())
    assertSingleError(errors, "class IntVector in package vector cannot be accessed")
  }

  @Test def addModules(): Unit = {
    assertNoErrors(compile(fileFactory(usesIncubator) :: Nil, fileFactory.tempDir(), extraArgs = List("--add-modules:jdk.incubator.vector")))
  }

  @Test def unnamedModuleCannotAccessUnexportedPackage(): Unit = {
    val code = fileFactory("class C { jdk.internal.misc.Unsafe.getUnsafe }")
    assertSingleError(compile(code :: Nil, fileFactory.tempDir()), "class Unsafe in package misc cannot be accessed")
  }

  @Test def addExportsToUnnamed(): Unit = {
    val code = fileFactory("class C { jdk.internal.misc.Unsafe.getUnsafe }")
    assertNoErrors(compile(code :: Nil, fileFactory.tempDir(), extraArgs = List("--add-exports:java.base/jdk.internal.misc=ALL-UNNAMED")))
  }

  @Test def jointCompilationWithModuleInfo(): Unit = {
    val moduleInfo = fileFactory("module-info.java",
      """module p1 {
        |  requires jdk.incubator.vector;
        |  requires scala.library;
        |  exports p1.api;
        |}
        |""".stripMargin)
    assertNoErrors(compile(fileFactory(usesIncubator) :: moduleInfo :: Nil, fileFactory.tempDir()))
  }

  @Test def jointCompilationWithModuleInfoMissingRequires(): Unit = {
    val moduleInfo = fileFactory("module-info.java",
      """module p1 {
        |  requires scala.library;
        |}
        |""".stripMargin)
    val errors = compile(fileFactory(usesIncubator) :: moduleInfo :: Nil, fileFactory.tempDir())
    assertSingleError(errors, "class IntVector in package vector cannot be accessed")
  }

  @Test def parseAndPrintModuleInfo(): Unit = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.usejavacp.value = true
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    new global.Run()
    val unit = new global.CompilationUnit(fileFactory("module-info.java",
      """import java.util.spi.ToolProvider;
        |@Deprecated
        |open module a.b.c {
        |  requires transitive java.sql;
        |  requires static java.desktop;
        |  requires java.compiler;
        |  requires static transitive java.logging;
        |  requires transitive;
        |  requires transitive.x;
        |  exports a.b.api;
        |  exports a.b.internal to x.y, z;
        |  opens a.b.res to x.y;
        |  uses java.util.spi.ToolProvider;
        |  provides ToolProvider with a.b.impl.MyTool, a.b.impl.MyTool2;
        |}
        |""".stripMargin))
    val printed = global.show(global.newJavaUnitParser(unit).parse())
    assertNoErrors(reporter.infos.toList.map(_.msg))
    val expected =
      """package <empty> {
        |  import _root_.java.util.spi.ToolProvider;
        |  open module a.b.c {
        |    requires transitive java.sql;
        |    requires static java.desktop;
        |    requires java.compiler;
        |    requires transitive static java.logging;
        |    requires transitive;
        |    requires transitive.x;
        |    exports a.b.api;
        |    exports a.b.internal to x.y, z;
        |    opens a.b.res to x.y;
        |    uses java.util.spi.ToolProvider;
        |    provides ToolProvider with a.b.impl.MyTool, a.b.impl.MyTool2
        |  }
        |}""".stripMargin
    assertEquals(expected, printed)
  }

  @Test def moduleInfoIgnoredWithoutYjpms(): Unit = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.processArguments(List("-d", fileFactory.tempDir().toString, "-usejavacp"), processAll = true)
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    val moduleInfo = fileFactory("module-info.java", "module p1 { requires java.sql; exports p1.api; }")
    new global.Run().compileSources(fileFactory("package p1.api; class C") :: moduleInfo :: Nil)
    assertNoErrors(reporter.infos.toList.map(_.msg))
  }

  @Test def namedModuleDoesNotReadClassPath(): Unit = {
    val libOut = fileFactory.tempDir()
    assertNoErrors(compile(fileFactory("lib/L.scala", "package lib; class L") :: Nil, libOut))
    val user = fileFactory("p1/U.scala", "package p1; class U { new lib.L }")

    // From the unnamed module, the class path is readable.
    assertNoErrors(compile(user :: Nil, fileFactory.tempDir(), extraArgs = List("-classpath", libOut.toString)))

    // A named module only reads the class path given --add-reads <module>=ALL-UNNAMED.
    val moduleInfo = fileFactory("module-info.java", "module p1 { requires scala.library; }")
    val errors = compile(user :: moduleInfo :: Nil, fileFactory.tempDir(), extraArgs = List("-classpath", libOut.toString))
    assertSingleError(errors, "class L in package lib cannot be accessed")
    assertNoErrors(compile(user :: moduleInfo :: Nil, fileFactory.tempDir(), extraArgs = List("-classpath", libOut.toString, "--add-reads:p1=ALL-UNNAMED")))
  }

  @Test def moduleGraphIsPerRun(): Unit = {
    val global = newGlobal(fileFactory.tempDir())
    val code = fileFactory(usesIncubator)
    def moduleInfo(requires: String*) =
      fileFactory("module-info.java", requires.map(r => s"requires $r;").mkString("module p1 { ", " ", " }"))

    assertSingleError(compileWith(global, code :: moduleInfo("scala.library") :: Nil), "class IntVector in package vector cannot be accessed")
    assertNoErrors(compileWith(global, code :: moduleInfo("scala.library", "jdk.incubator.vector") :: Nil))
    // Without a module-info.java, the module-info.class written by the previous run makes the sources members of p1,
    // as in an incremental compilation that doesn't recompile module-info.java.
    assertNoErrors(compileWith(global, code :: Nil))
    // A module-info.java among the sources takes precedence over the module-info.class in the output.
    assertSingleError(compileWith(global, code :: moduleInfo("scala.library") :: Nil), "class IntVector in package vector cannot be accessed")
  }

  @Test def secondModuleInfoInRunIsAnError(): Unit = {
    val m1 = fileFactory("m1/module-info.java", "module m1 { }")
    val m2 = fileFactory("m2/module-info.java", "module m2 { }")
    assertSingleError(compile(m1 :: m2 :: Nil, fileFactory.tempDir()), "module-info.java declares a second module in this compilation run")
  }

  @Test def unnamedModuleReadsDefaultRootModules(): Unit = {
    // java.sql is not required by java.base, but is a default root module for code in the unnamed module (JEP 261)
    assertNoErrors(compile(fileFactory("class C { def c: java.sql.Connection = null }") :: Nil, fileFactory.tempDir()))
  }

  @Test def checksJdkInternalsWithoutModulePath(): Unit = {
    Assume.assumeTrue("the javac class path is only used with --module-path", classPathImpl == "native")
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.processArguments(List("-Yjpms", "-usejavacp", "-d", fileFactory.tempDir().toString), processAll = true)
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    new global.Run().compileSources(fileFactory("class C { jdk.internal.misc.Unsafe.getUnsafe; java.util.List.of() }") :: Nil)
    assertSingleError(reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg), "class Unsafe in package misc cannot be accessed")
  }

  @Test def macroFromModulePath(): Unit = {
    Assume.assumeTrue("JpmsClassPath.asURLs is not implemented (PLAN.md, Track 2 step 1)", classPathImpl == "native")
    val macroOut = fileFactory.tempDir()
    val macroLib = fileFactory("mac/M.scala",
      """package mac
        |import scala.language.experimental.macros
        |import scala.reflect.macros.blackbox.Context
        |object M {
        |  def m: Int = macro impl
        |  def impl(c: Context): c.Expr[Int] = c.universe.reify(42)
        |}
        |""".stripMargin)
    // From the unnamed module, modules on the module path are only resolved if named in --add-modules (JEP 261).
    assertNoErrors(compile(macroLib :: Nil, macroOut, extraArgs = List("-usejavacp", "--add-modules:scala.library")))
    val macroJar = createAutomaticModuleJar(fileFactory.tempDir().resolve("mac.jar"), "mac", contentsOf = Some(macroOut))
    // The macro class loader is built from the class path URLs, which must include the module path.
    val urls = newGlobal(fileFactory.tempDir(), List(macroJar)).classPath.asURLs
    assertTrue(urls.toString, urls.contains(macroJar.toUri.toURL))
    val client = fileFactory("client/Client.scala", "package client; class Client { def x = mac.M.m }")
    assertNoErrors(compile(client :: Nil, fileFactory.tempDir(), List(macroJar), List("-usejavacp", "--add-modules:scala.library,mac")))
  }

  @Test def separateCompilationEndToEnd(): Unit = {
    // Module `a`, compiled by scalac, including its module-info.java.
    val aOut = fileFactory.tempDir()
    val aSources = List(
      fileFactory("a/api/Api.scala", "package a.api; class Api { def hello: String = a.impl.Impl.greeting }"),
      fileFactory("a/impl/Impl.scala", "package a.impl; object Impl { def greeting = \"hello from a\" }"),
      fileFactory("a/spi/Greeter.scala", "package a.spi; trait Greeter { def greet: String }"),
      fileFactory("a/impl/DefaultGreeter.scala", "package a.impl; class DefaultGreeter extends a.spi.Greeter { def greet = \"greetings from a\" }"),
      fileFactory("a/module-info.java",
        """import a.spi.Greeter;
          |module a {
          |  requires transitive scala.library;
          |  requires static java.sql;
          |  exports a.api;
          |  exports a.spi;
          |  opens a.impl to b;
          |  uses Greeter;
          |  provides Greeter with a.impl.DefaultGreeter;
          |}
          |""".stripMargin))
    assertNoErrors(compile(aSources, aOut))

    val descriptor = java.lang.module.ModuleFinder.of(aOut).find("a").get.descriptor
    assertEquals(
      """module { name: a, [mandated java.base, static java.sql, transitive scala.library], uses: [a.spi.Greeter], exports: [a.api, a.spi], opens: [a.impl to [b]], provides: [a.spi.Greeter with [a.impl.DefaultGreeter]] }""",
      canonical(descriptor))

    // Module `b`, compiled by scalac against `a` on the module path. It reads scala.library through `requires transitive`.
    val bOut = fileFactory.tempDir()
    val bMain = fileFactory("b/main/Main.scala",
      """package b.main
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    println(new a.api.Api().hello)
        |    println(java.util.ServiceLoader.load(classOf[a.spi.Greeter]).findFirst.get.greet)
        |  }
        |}
        |""".stripMargin)
    val bModuleInfo = fileFactory("b/module-info.java", "module b { requires a; uses a.spi.Greeter; }")
    assertNoErrors(compile(bMain :: bModuleInfo :: Nil, bOut, List(aOut)))
    val bInternal = fileFactory("b/bad/Bad.scala", "package b.bad; class Bad { a.impl.Impl.greeting }")
    assertSingleError(compile(bInternal :: bModuleInfo :: Nil, fileFactory.tempDir(), List(aOut)), "object Impl in package impl cannot be accessed")

    // A Java module, compiled by javac against `a`.
    val javacModulePath = List(scalaLibraryModule, aOut).mkString(File.pathSeparator)
    val javacOptions = List("--module-path", javacModulePath, "--patch-module", s"scala.library=$libraryClasses")
    val cInfo = fileFactory("c/module-info.java", "module c { requires a; }")
    val cGood = fileFactory("c/c/C.java", "package c; public class C { public static String hello() { return new a.api.Api().hello(); } }")
    assertEquals("", javac(cInfo :: cGood :: Nil, fileFactory.tempDir(), javacOptions))
    val cBad = fileFactory("c/c/CBad.java", "package c; public class CBad { Object o = a.impl.Impl.greeting(); }")
    val javacErrors = javac(cInfo :: cBad :: Nil, fileFactory.tempDir(), javacOptions)
    assertTrue(javacErrors, javacErrors.contains("package a.impl is declared in module a, which does not export it"))

    // scalac against a module compiled by javac.
    val dOut = fileFactory.tempDir()
    val dSources = List(
      fileFactory("d/module-info.java", "module d { exports d.api; }"),
      fileFactory("d/d/api/D.java", "package d.api; public class D { public static String hello() { return \"hello from d\"; } }"),
      fileFactory("d/d/internal/Hidden.java", "package d.internal; public class Hidden { }"))
    assertEquals("", javac(dSources, dOut, Nil))
    val eInfo = fileFactory("e/module-info.java", "module e { requires d; requires scala.library; }")
    assertNoErrors(compile(fileFactory("e/E.scala", "package e; class E { d.api.D.hello() }") :: eInfo :: Nil, fileFactory.tempDir(), List(dOut)))
    assertSingleError(compile(fileFactory("e/EBad.scala", "package e; class EBad { new d.internal.Hidden }") :: eInfo :: Nil, fileFactory.tempDir(), List(dOut)),
      "class Hidden in package internal cannot be accessed")

    // Finally, run `b` on the module path.
    val javaExe = Paths.get(sys.props("java.home"), "bin", "java").toString
    val runtimeModulePath = List(scalaLibraryModule, aOut, bOut).mkString(File.pathSeparator)
    val process = new ProcessBuilder(javaExe, "--module-path", runtimeModulePath, "--patch-module", s"scala.library=$libraryClasses", "-m", "b/b.main.Main")
      .redirectErrorStream(true).start()
    val output = new String(process.getInputStream.readAllBytes()).trim
    assertEquals(output, 0, process.waitFor())
    assertEquals("hello from a\ngreetings from a", output.replace("\r\n", "\n"))
  }

  /** `ModuleDescriptor.toString`, with the set-valued components sorted. */
  private def canonical(d: java.lang.module.ModuleDescriptor): String = {
    import java.lang.module.ModuleDescriptor._
    def mods(ms: java.util.Set[_ <: AnyRef]) = ms.asScala.map(_.toString.toLowerCase).toList.sorted.map(_ + " ").mkString
    def tos(ts: java.util.Set[String]) = if (ts.isEmpty) "" else ts.asScala.toList.sorted.mkString(" to [", ", ", "]")
    val requires = d.requires.asScala.toList.map((r: Requires) => mods(r.modifiers) + r.name).sorted
    val exports = d.exports.asScala.toList.map((e: Exports) => e.source + tos(e.targets)).sorted
    val opens = d.opens.asScala.toList.map((o: Opens) => o.source + tos(o.targets)).sorted
    val provides = d.provides.asScala.toList.map((p: Provides) => p.service + p.providers.asScala.sorted.mkString(" with [", ", ", "]")).sorted
    s"module { name: ${d.name}, ${requires.mkString("[", ", ", "]")}, uses: ${d.uses.asScala.toList.sorted.mkString("[", ", ", "]")}, " +
      s"exports: ${exports.mkString("[", ", ", "]")}, opens: ${opens.mkString("[", ", ", "]")}, provides: ${provides.mkString("[", ", ", "]")} }"
  }

  /** Compiles Java sources with javac, returning the diagnostics. */
  private def javac(sources: List[SourceFile], outputDir: Path, options: List[String]): String = {
    val compiler = javax.tools.ToolProvider.getSystemJavaCompiler
    val out = new java.io.StringWriter
    val fileManager = compiler.getStandardFileManager(null, null, null)
    try {
      val units = fileManager.getJavaFileObjects(sources.map(_.file.file): _*)
      compiler.getTask(out, fileManager, null, ("-d" :: outputDir.toString :: options).asJava, null, units).call()
    } finally fileManager.close()
    out.toString.trim
  }

  @Test def invalidModuleDeclarations(): Unit = {
    val code = fileFactory("p1/C.scala", "package p1; class C")
    def errors(decl: String) = compile(code :: fileFactory("module-info.java", decl) :: Nil, fileFactory.tempDir())
    assertSingleError(errors("module p1 { requires scala.library; requires scala.library; }"), "Dependence upon scala.library already declared")
    assertSingleError(errors("module p1 { requires scala.library; exports p1; exports p1; }"), "Exported package p1 already declared")
    assertSingleError(errors("module p1 { requires scala.library; uses p1.Missing; }"), "type p1.Missing not found in module declaration")
  }

  @Test def clientsOfNamedModule(): Unit = {
    val p1Out = fileFactory.tempDir()
    assertNoErrors(compile(fileFactory(usesIncubator) :: Nil, p1Out, extraArgs = List("--add-modules:jdk.incubator.vector")))
    // scalac does not (yet) emit module-info.class, write it with ASM.
    writeModuleInfo(p1Out, "p1", requires = List("jdk.incubator.vector", "scala.library"), exports = List("p1.api"), packages = List("p1.api", "p1.impl"))

    val clientOut = fileFactory.tempDir()

    // The unnamed module reads all modules, so can access the exported p1.api.
    assertNoErrors(compile(fileFactory("class Client { new p1.api.P1Api }") :: Nil, clientOut, List(p1Out), List("--add-modules:p1")))

    // ... but not the unexported p1.impl
    val clientCode = fileFactory("other/Patched.scala", "package other; class Patched { new p1.api.P1Api; new p1.impl.C }")
    assertSingleError(compile(clientCode :: Nil, clientOut, List(p1Out), List("--add-modules:p1")), "class C in package impl cannot be accessed")

    // We can add the export on the command line
    assertNoErrors(compile(clientCode :: Nil, clientOut, List(p1Out), List("--add-modules:p1", "--add-exports:p1/p1.impl=ALL-UNNAMED")))

    // Or, as for whitebox tests, patch the client sources into module `p1`, giving them access to its internals.
    assertNoErrors(compile(clientCode :: Nil, clientOut, List(p1Out), List("--add-modules:p1", s"--patch-module:p1=${fileFactory.baseDirectory}")))
  }

  private def writeModuleInfo(dir: Path, name: String, requires: List[String], exports: List[String], packages: List[String]): Unit = {
    def internal(pkg: String) = pkg.replace('.', '/')
    val cw = new ClassWriter(0)
    cw.visit(Opcodes.V9, Opcodes.ACC_MODULE, "module-info", null, null, null)
    val mv = cw.visitModule(name, 0, null)
    mv.visitRequire("java.base", Opcodes.ACC_MANDATED, null)
    requires.foreach(mv.visitRequire(_, 0, null))
    exports.foreach(e => mv.visitExport(internal(e), 0))
    packages.foreach(p => mv.visitPackage(internal(p)))
    mv.visitEnd()
    cw.visitEnd()
    Files.write(dir.resolve("module-info.class"), cw.toByteArray)
  }

  private def createAutomaticModuleJar(jar: Path, automaticModuleName: String, contentsOf: Option[Path] = None): Path = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(new Name("Automatic-Module-Name"), automaticModuleName)
    val os = new ByteArrayOutputStream()
    manifest.write(os)
    val env = java.util.Collections.singletonMap("create", "true")
    val zipfs = FileSystems.newFileSystem(java.net.URI.create("jar:" + jar.toUri), env)
    try {
      val target = zipfs.getPath("/META-INF/MANIFEST.MF")
      Files.createDirectories(target.getParent)
      Files.write(target, os.toByteArray)
      for (dir <- contentsOf; file <- Files.walk(dir).iterator.asScala if Files.isRegularFile(file)) {
        val entry = zipfs.getPath("/" + dir.relativize(file).toString)
        Files.createDirectories(entry.getParent)
        Files.copy(file, entry)
      }
    } finally zipfs.close()
    jar
  }
}
