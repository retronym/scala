import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.BatchSourceFile

// Instantiates three distinct anonymous subclasses of Global and compiles a trivial source with each.
object ThreeGlobals {
  def compile(mk: (Settings, ConsoleReporter) => Global): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val g = mk(settings, new ConsoleReporter(settings))
    val run = new g.Run
    run.compileSources(List(new BatchSourceFile("A.scala", "class A { def f = 1 }")))
    println(s"compiled with ${g.getClass.getName}")
  }
  def main(args: Array[String]): Unit = {
    compile(new Global(_, _) {})
    compile(new Global(_, _) {})
    compile(new Global(_, _) {})
  }
}
