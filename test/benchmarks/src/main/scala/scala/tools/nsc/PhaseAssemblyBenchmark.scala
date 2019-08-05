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
  class P[G <: Global with Singleton](g: G) extends Plugin() {
    override val name: String = "plugin"
    override val components: List[PluginComponent] = List(
      new component("phase_0", List("parser"), List("namer")),
      new component("phase_1", List("parser", "phase_0"), List("namer")),
      new component("phase_2", List("typer"), List("refchecks")),
      new component("phase_3", List("phase_1"), List("namer")),
      new component("phase_4", List("typer"), List("phase_7")),
      new component("phase_5", List("typer"), List("phase_6", "phase_13")),
      new component("phase_6", List("typer"), List("phase_11", "phase_13")),
      new component("phase_7", List("pickler", "phase_9"), List("phase_19" /*, "patmat"*/)),
      new component("phase_8", List("typer"), List("phase_9", "phase_13", "phase_11")),
      new component("phase_9", List("typer", "phase_8"), List("superaccessors", "pickler", "phase_17", "phase_13", "phase_11", "phase_7")),
      new component("phase_10", List("typer"), List("refchecks")),
      new component("phase_11", List("typer"), List("phase_14")),
      new component("phase_12", List("typer"), List("explicitouter")),
      new component("phase_13", List("typer"), List("phase_14", "phase_16")),
      new component("phase_14", List("typer"), List("superaccessors")),
      new component("phase_15", List("typer", "phase_14"), List("refchecks")),
      new component("phase_16", List("typer", "phase_14"), List("refchecks")),
      new component("phase_17", List("phase_9", "phase_14", "patmat"), List("phase_18", "phase_19")),
      new component("phase_18", List("phase_16"), List("refchecks")),
      new component("phase_19", List("phase_18", "patmat", "refchecks"), List("explicitouter")),
      new component("phase_20", List("mixin"), List("cleanup")),
      new component("phase_21", List("phase_20"), List("cleanup")),
      new component("phase_22", List("phase_21"), List("cleanup")),
      new component("phase_23", List("delambdafy"), List("jvm"))
    )
    class component(override val phaseName: String, override val runsAfter: List[String], override val runsBefore: List[String]) extends PluginComponent {
      override val global: G = g
      override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
        override def apply(unit: global.CompilationUnit): Unit = ()
      }
    }
    override val description: String = ""
    override val global: G = g
  }

  @Setup
  def setup(): Unit = {
    val global = new Global(new Settings) {
      override def loadPlugins(): List[Plugin] = List(new P[this.type](this))
    }
    global.settings.usejavacp.value = true
    g = global
  }

  @Benchmark def newRun(): Object = {
    val global = g
    import global._
    val r = new Run
    val numCustomPhases = r.phaseNamed("parser").iterator.count(_.name.startsWith("phase_"))
//    Predef.assert(numCustomPhases == 24, (numCustomPhases , r.phaseNamed("parser").iterator.map(_.name).mkString(",")))
    r
  }
}

object PhaseAssemblyBenchmark {
  def main(args: Array[String]): Unit = {
    val bench = new PhaseAssemblyBenchmark
    bench.setup()
    bench.newRun()
    bench.newRun()
  }
}