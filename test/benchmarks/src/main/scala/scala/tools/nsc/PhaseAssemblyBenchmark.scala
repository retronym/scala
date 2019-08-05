package scala.tools.nsc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh

import scala.tools.nsc.plugins.{Plugin, PluginComponent}

@BenchmarkMode(Array(jmh.annotations.Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class PhaseAssemblyBenchmark {
  var g: Global = _
  class P[G <: Global with Singleton](g: G, i: Int) extends Plugin() {
    private def nameOf(i: Int) = f"plugin${i}%03d"
    override val name: String = nameOf(i)
    override val components: List[PluginComponent] = List(component)
    object component extends PluginComponent {
      override val global: G = g
      override val phaseName: String = P.this.name
      override val runsAfter: List[String] = List(if (i == 0) "typer" else nameOf(i - 1))
      override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
        override def apply(unit: global.CompilationUnit): Unit = ()
      }
    }
    override val description: String = ""
    override val global: G = g
  }
  @Param(Array("1", "5", "10", "15", "20"))
  var size: Int = 1

  @Setup
  def setup(): Unit = {
    val global = new Global(new Settings) {
      override def loadPlugins(): List[Plugin] = List.tabulate(size)(i => new P[this.type](this, i))
    }
    global.settings.usejavacp.value = true
    g = global
  }

  @Benchmark def newRun(): Object = {
    val global = g
    import global._
    val r = new Run
    val numPostTyperPhases = r.typerPhase.iterator.count(_.name.startsWith("plugin"))
    Predef.assert(numPostTyperPhases == size, r.typerPhase.iterator.map(_.name).mkString(","))
    r
  }
}
