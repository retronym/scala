package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

/** Compile the same sources in several runs of one Global, as resident compilers do. */
@RunWith(classOf[JUnit4])
class MultiRunTest {
  private def messagesPerRun(runs: Int, options: String, sources: (String, String)*): List[List[String]] = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.processArgumentString(options)
    settings.stopAfter.value = List("refchecks")
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    List.fill(runs) {
      reporter.reset()
      new global.Run().compileSources(sources.toList.map { case (name, code) => new BatchSourceFile(name, code) })
      reporter.infos.toList.map(_.msg)
    }
  }

  // `Object` in a Java source must be `ObjectTpeJava`, which accepts `Any`, in every run.
  // `checkAccessible` compared the type with `definitions.ObjectTpe` by `eq`, but that `lazy val`
  // holds the instance from the run that forced it, and each run has a new uniques table.
  @Test def javaObjectIsAnyInLaterRuns(): Unit = {
    val java  = "K.java"  -> "package p; public class K { public Object f(Object o) { return o; } }"
    val scala = "T.scala" -> "object T { def t(k: p.K, a: Any): Any = k.f(a) }"
    assertEquals(List(Nil, Nil, Nil), messagesPerRun(3, "", java, scala))
  }

  // `-Xlint:universal-methods` compared the qualifier's type with `definitions.UnitTpe` by `eq`, so it
  // only warned in the run that forced that `lazy val`.
  @Test def dubiousUnitSelectionInLaterRuns(): Unit = {
    val warning = List("dubious usage of method hashCode with unit value")
    val code = "C.scala" -> "class C { def f(): Unit = (); def g = f().hashCode }"
    assertEquals(List(warning, warning, warning), messagesPerRun(3, "-Xlint:universal-methods", code))
  }

  // A self type with the class's own type constructor is no self type. `hasSelfType` compared the type
  // constructors with `ne`, which only works if both come from the same uniques table. Code that runs after
  // the end-of-run clear, still in that run (as the REPL does), builds one of them in a new table.
  @Test def selfTypeOfOwnTypeConstructorAfterClear(): Unit = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val reporter = new StoreReporter(settings)
    val global = new Global(settings, reporter)
    import global._
    val run = new Run()
    run.compileSources(List(new BatchSourceFile("F.scala", "trait F[A] { self: F[A] => }")))
    assertEquals(Nil, reporter.infos.toList)
    val F = run.units.next().body.collect { case cd: ClassDef => cd.symbol }.head
    assertFalse(F.hasSelfType)
    perRunCaches.clearAll()
    assertFalse(F.hasSelfType)
  }
}
