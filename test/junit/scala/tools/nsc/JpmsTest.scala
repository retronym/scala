package scala.tools.nsc

import java.io.{ByteArrayOutputStream, File}
import java.nio.file._
import java.util.jar.Attributes.Name
import java.util.jar.Manifest

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.{After, Before, Test}

import scala.reflect.internal.util.SourceFile
import scala.tools.asm.{ClassWriter, Opcodes}
import scala.tools.nsc.reporters.StoreReporter

class JpmsTest {
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

  private def compile(sources: List[SourceFile], outputDir: Path, modulePath: List[Path] = Nil, extraArgs: List[String] = Nil): List[String] = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    val fullModulePath = (scalaLibraryModule :: modulePath).mkString(File.pathSeparator)
    val args = List("-Yjpms", "-d", outputDir.toString, "--module-path", fullModulePath, s"--patch-module:scala.library=$libraryClasses") ++ extraArgs
    val (ok, residual) = settings.processArguments(args, processAll = true)
    assertTrue(args.toString, ok && residual.isEmpty)
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    new global.Run().compileSources(sources)
    reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg)
  }

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

  private def createAutomaticModuleJar(jar: Path, automaticModuleName: String): Path = {
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
    } finally zipfs.close()
    jar
  }
}
