package scala.tools.nsc

import java.nio.file.{Files, Path}

import org.junit.Assert.assertEquals
import org.junit.{After, Before, Test}

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.FileUtils.deleteRecursive
import scala.tools.nsc.reporters.StoreReporter

/** A `Global` kept across runs compiles like a fresh one, although lowering writes to the symbols of the class path:
 *  the names and flags written in one run are undone when the next starts.
 */
class KeptGlobalTest {
  private var base: Path = _

  @Before def before(): Unit = base = Files.createTempDirectory("keptGlobalTest")
  @After def after(): Unit = if (base != null) deleteRecursive(base)

  private val librarySource =
    """package p
      |trait T { private val v = 1; private object O; def f: Any = (v, O) }
      |class Outer { class Inner }
      |""".stripMargin

  private def newGlobal(classpath: Path): (Global, StoreReporter) = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.classpath.value = classpath.toString
    val reporter = new StoreReporter(settings)
    (new Global(settings, reporter), reporter)
  }

  /** Compile `code` into a new directory on `g`; the class files by name and the diagnostics. */
  private def compile(g: Global, reporter: StoreReporter, code: String): (Map[String, List[Byte]], List[String]) = {
    val out = Files.createTempDirectory(base, "out")
    g.settings.outputDirs.setSingleOutput(out.toString)
    reporter.reset()
    new g.Run().compileSources(List(new BatchSourceFile("Test.scala", code)))
    val classes = Files.walk(out).iterator.asScala.filter(_.toString.endsWith(".class"))
      .map(f => out.relativize(f).toString -> Files.readAllBytes(f).toList).toMap
    (classes, reporter.infos.toList.map(i => s"${i.severity} ${i.pos.line}: ${i.msg}"))
  }

  private def library(): Path = {
    val (g, reporter) = newGlobal(base)
    val (classes, infos) = compile(g, reporter, librarySource)
    assert(classes.nonEmpty && infos.isEmpty, infos)
    g.settings.outputDirs.getSingleOutput.get.file.toPath
  }

  private def freshCompile(lib: Path, code: String) = { val (g, r) = newGlobal(lib); compile(g, r, code) }

  private def assertSameClasses(expected: Map[String, List[Byte]], actual: Map[String, List[Byte]]): Unit = {
    assertEquals(expected.keySet, actual.keySet)
    val differing = expected.keys.filter(k => expected(k) != actual(k)).toList.sorted
    assertEquals("class files that differ from a fresh Global's", Nil, differing)
  }

  @Test def bytesOfSecondRunMatchFresh(): Unit = {
    val lib = library()
    val code = "class C extends p.T"
    val (kept, reporter) = newGlobal(lib)
    compile(kept, reporter, code)
    val (second, infos) = compile(kept, reporter, code)
    val (fresh, freshInfos) = freshCompile(lib, code)
    assertEquals(freshInfos, infos)
    assertSameClasses(fresh, second)
  }

  @Test def diagnosticsOfSecondRunMatchFresh(): Unit = {
    val lib = library()
    val (kept, reporter) = newGlobal(lib)
    compile(kept, reporter, "class C extends p.T")
    val lookup = "class C extends p.T { def g(t: p.T) = t.v }"
    val (_, infos) = compile(kept, reporter, lookup)
    val (_, freshInfos) = freshCompile(lib, lookup)
    assert(freshInfos.nonEmpty)
    assertEquals(freshInfos, infos)
  }

  @Test def liftedClassesMatchFresh(): Unit = {
    val lib = library()
    val code = "class D extends p.Outer { def i(o: p.Outer) = new o.Inner }"
    val (kept, reporter) = newGlobal(lib)
    compile(kept, reporter, code)
    val (second, infos) = compile(kept, reporter, code)
    val (fresh, freshInfos) = freshCompile(lib, code)
    assertEquals(freshInfos, infos)
    assertSameClasses(fresh, second)
  }
}
