package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

/** Compile the same sources in several runs of one Global, as resident compilers do. */
@RunWith(classOf[JUnit4])
class MultiRunTest {
  private def errorsPerRun(runs: Int, sources: (String, String)*): List[List[String]] = {
    val settings = new Settings
    settings.usejavacp.value = true
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
    assertEquals(List(Nil, Nil, Nil), errorsPerRun(3, java, scala))
  }
}
