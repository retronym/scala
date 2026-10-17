package scala.tools.nsc.compilerlab

import java.util.concurrent.TimeUnit

import io.github.retronym.perflab.jmh.{Announce, IterationLog, Variants}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.IterationParams
import org.openjdk.jmh.runner.IterationType

import scala.jdk.CollectionConverters._
import scala.reflect.internal.NoPhase
import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

/** The fixed cost of a typing transformer, apart from what a phase does with the trees: one op runs a
 *  `TypingTransformer` that changes nothing over every unit of a corpus, compiled (in setup, once) up to `stopAfter`.
 *  It enters each owner as a phase's transformer does, and with `typedEvery` = N also types a unit literal at every
 *  N-th method, as a phase that types the odd tree it makes; 0 types nothing. The trees are never replaced, so every op
 *  sees the same ones.
 *
 *  The transformers, one per unit with its root typer, are made in setup and reused: a pass that types nothing leaves
 *  each as it found it. So with `typedEvery` = 0 an op allocates only what entering owners does, which with lazy
 *  contexts should be nothing.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms4G", "-Xmx4G", "-Xss2M"))
@Warmup(iterations = 10, time = 2)
@Measurement(iterations = 10, time = 2)
class TypingTransformerBenchmark {
  @Param(Array("re2s"))
  var corpus: String = _

  /** The phase the setup compiles up to; the transformer runs at the phase after it, on its trees. */
  @Param(Array("typer"))
  var stopAfter: String = _

  /** Type a unit literal in the context of every N-th method; 0: type nothing. */
  @Param(Array("0", "1", "16"))
  var typedEvery: Int = _

  /** Extra compiler options, space separated. */
  @Param(Array(""))
  var scalacOptions: String = _

  /** Variants compared within the fork, as `CompileBenchmark`'s: each its own `Global`, compiled with its options. */
  @Param(Array(""))
  var variants: String = _

  /** As `CompileBenchmark`'s. */
  @Param(Array(""))
  var jdkHome: String = _

  private var pass: TypingTransformerBenchmark.Pass = _
  private var parsed: Variants = _
  private var alternatives: Map[String, TypingTransformerBenchmark.Pass] = Map.empty
  private var iteration = 0

  @Setup(Level.Trial)
  def setup(): Unit = {
    Announce.jvm(s"TypingTransformerBenchmark($corpus)")
    if (jdkHome.nonEmpty) {
      val home =
        if (jdkHome.contains('/')) jdkHome
        else s"${sys.env.getOrElse("SDKMAN_DIR", sys.props("user.home") + "/.sdkman")}/candidates/java/$jdkHome"
      System.setProperty("compilerlab.jdkHome", home)
    }
    val src = Corpus.load(corpus)
    val common = s"-Ystop-after:$stopAfter" :: CompileBenchmark.split(scalacOptions)
    def prepare(options: List[String]) = {
      val compiler = new InMemoryCompiler(src, options, resident = true)
      compiler.compile()
      val p = new TypingTransformerBenchmark.Pass(compiler.residentGlobal, stopAfter, typedEvery)
      println(s"\n[compiler-lab] $corpus up to $stopAfter (${options.mkString(" ")}): ${p.units} units; one pass: ${p.run()}")
      p
    }
    parsed = Variants.parse(variants)
    if (parsed.isEmpty) pass = prepare(common)
    else alternatives = parsed.all.asScala.map(v => v.name -> prepare(common ::: v.options.asScala.toList)).toMap
    IterationLog.reset(parsed.names)
    iteration = 0
  }

  @Setup(Level.Iteration)
  def nextVariant(ip: IterationParams): Unit =
    if (!parsed.isEmpty) {
      val v = parsed.forIteration(iteration)
      iteration += 1
      pass = alternatives(v.name)
      IterationLog.begin(v.name, ip.getType == IterationType.MEASUREMENT)
    }

  @TearDown(Level.Iteration)
  def endVariant(): Unit = if (!parsed.isEmpty) IterationLog.end()

  /** Returns the number of literals typed, which depends on `typedEvery` only. */
  @Benchmark
  def transform(): Int = pass.run()
}

object TypingTransformerBenchmark {
  /** A typing transformer over the units a compilation left in `global.currentRun`. */
  final class Pass(val global: Global, stopAfter: String, typedEvery: Int) extends TypingTransformers {
    import global._

    private val compiled = currentRun
    private val at = {
      val stopped = compiled.phaseNamed(stopAfter)
      if (stopped eq NoPhase) sys.error(s"no phase $stopAfter")
      if (stopped.next eq NoPhase) stopped else stopped.next
    }
    def units: Int = probes.length

    private final class Probe(unit: CompilationUnit) extends TypingTransformer(unit) {
      private var methods = 0
      var typed = 0
      override def transform(tree: Tree): Tree = tree match {
        case _: DefDef if typedEvery > 0 =>
          methods += 1
          if (methods % typedEvery == 0) {
            localTyper.typed(Literal(Constant(())))
            typed += 1
          }
          super.transform(tree)
        case _ => super.transform(tree)
      }
      def pass(): Int = {
        methods = 0
        typed = 0
        transform(unit.body)
        typed
      }
    }

    private val probes: Array[Probe] = enteringPhase(at)(compiled.units.map(new Probe(_)).toArray)

    /** One pass over every unit; the number of literals typed. Not `enteringPhase`, whose by-name argument would be the
     *  op's one allocation. */
    def run(): Int = {
      val saved = pushPhase(at)
      try {
        var typed, i = 0
        while (i < probes.length) {
          typed += probes(i).pass()
          i += 1
        }
        typed
      } finally popPhase(saved)
    }
  }
}
