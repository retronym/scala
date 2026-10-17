package scala.tools.nsc.compilerlab

import java.util.concurrent.TimeUnit

import io.github.retronym.perflab.jmh.{Announce, IterationLog, Variants}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.IterationParams
import org.openjdk.jmh.runner.IterationType

import scala.jdk.CollectionConverters._

/** One op is one full compilation of a corpus, from scratch in a warm JVM, with no file I/O (see `InMemoryCompiler`). */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms4G", "-Xmx4G", "-Xss2M"))
@Warmup(iterations = 20, time = 3)
@Measurement(iterations = 10, time = 3)
class CompileBenchmark {
  import CompileBenchmark.BuildOption

  @Param(Array("scalap", "re2s", "scala"))
  var corpus: String = _

  /** Extra compiler options, space separated, e.g. `-Ystop-after:typer`. */
  @Param(Array(""))
  var scalacOptions: String = _

  /** `fresh`: a new `Global` per op; `resident`: one `Global` for all ops, a new `Run` per op. */
  @Param(Array("fresh"))
  var global: String = _

  /** Variants compared within the fork, `<name>=<scalacOptions>|<name>=<scalacOptions>...`, the first the baseline.
   *  Each iteration, warmup included, runs the next variant in a Williams order (perflab's `Variants`), so the variants
   *  share the fork's compiled code; `PairedProfiler` and `XctraceProfiler` report per variant. The options add to
   *  `scalacOptions`. JMH splits parameter values at commas: none here.
   *
   *  A variant's options may name a build instead of (or as well as) flags: `@build=<checkout>` compiles with that
   *  checkout's build of reflect and the compiler, in its own class loader (`IsolatedBuild`), e.g.
   *  `base=@build=../base|new=@build=.`; a relative checkout resolves against this one.
   */
  @Param(Array(""))
  var variants: String = _

  /** The JDK whose API the corpus compiles against, if not the running one (`compilerlab.jdkHome`): e.g. an older JDK
   *  for a corpus that clashes with newer API, as compiler-benchmark's `scala` does with JDK 26's `Comparator.max`.
   *  A name without `/` is an SDKMAN JDK (`$SDKMAN_DIR/candidates/java/<name>`), which keeps the value usable in the
   *  file names profilers derive from parameters.
   */
  @Param(Array(""))
  var jdkHome: String = _

  /** With `variants`: a full collection before each iteration (outside the measured region). */
  @Param(Array("false"))
  var gcBetweenIterations: Boolean = false

  private var compiler: Compiling = _
  private var parsed: Variants = _
  private var alternatives: Map[String, Compiling] = Map.empty
  private var iteration = 0

  @Setup(Level.Trial)
  def setup(): Unit = {
    Announce.jvm(s"CompileBenchmark($corpus)")
    if (jdkHome.nonEmpty) {
      val home =
        if (jdkHome.contains('/')) jdkHome
        else s"${sys.env.getOrElse("SDKMAN_DIR", sys.props("user.home") + "/.sdkman")}/candidates/java/$jdkHome"
      System.setProperty("compilerlab.jdkHome", home)
    }
    val common = CompileBenchmark.split(scalacOptions)
    val src = Corpus.load(corpus)
    val resident = global match {
      case "fresh" => false
      case "resident" => true
      case other => sys.error(s"global=$other: expected fresh or resident")
    }
    parsed = Variants.parse(variants)
    val own = new InMemoryCompiler(src, common, resident)
    println(s"\n[compiler-lab] $corpus: ${src.files.size} sources; classpath in memory: ${own.classpathSnapshot}")
    if (parsed.isEmpty) {
      compiler = own
      compiler.compile()
      println(s"[compiler-lab] ${compiler.outputCount} output files")
    }
    alternatives = parsed.all.asScala.map { v =>
      val (builds, options) = v.options.asScala.toList.partition(_.startsWith(BuildOption))
      val c = builds.map(_.stripPrefix(BuildOption)) match {
        case Nil => new InMemoryCompiler(src, common ::: options, resident)
        case build :: Nil => IsolatedBuild(build).newCompiler(corpus, common ::: options, resident)
        case _ => sys.error(s"variant ${v.name}: more than one $BuildOption")
      }
      c.compile()
      println(s"[compiler-lab] variant ${v.name}: ${v.options.asScala.mkString(" ")} -> ${c.outputCount} output files")
      v.name -> c
    }.toMap
    IterationLog.reset(parsed.names)
    iteration = 0
  }

  @Setup(Level.Iteration)
  def nextVariant(ip: IterationParams): Unit =
    if (!parsed.isEmpty) {
      val v = parsed.forIteration(iteration)
      iteration += 1
      if (gcBetweenIterations) System.gc()
      compiler = alternatives(v.name)
      IterationLog.begin(v.name, ip.getType == IterationType.MEASUREMENT)
    }

  @TearDown(Level.Iteration)
  def endVariant(): Unit = if (!parsed.isEmpty) IterationLog.end()

  @Benchmark
  def compile(): Unit = compiler.compile()
}

object CompileBenchmark {
  private final val BuildOption = "@build="
  private[compilerlab] def split(options: String): List[String] = options.split(" ").toList.filter(_.nonEmpty)
}

/** Compiles a corpus repeatedly outside JMH, printing each run's time and the live heap after it (after a full GC),
 *  e.g. to attach a profiler or Instruments.
 *
 *  `CompileLoop [corpus] [runs] [scalac options...]`; `-Dcompilerlab.resident` shares one `Global` across runs.
 */
object CompileLoop {
  def main(args: Array[String]): Unit = {
    val corpus = args.headOption.getOrElse("scalap")
    val n = args.lift(1).fold(20)(_.toInt)
    val compiler = new InMemoryCompiler(Corpus.load(corpus), args.drop(2).toList, resident = sys.props.contains("compilerlab.resident"))
    println(s"classpath in memory: ${compiler.classpathSnapshot}")
    val memory = java.lang.management.ManagementFactory.getMemoryMXBean
    for (i <- 1 to n) {
      val t0 = System.nanoTime
      compiler.compile()
      val ms = (System.nanoTime - t0) / 1e6
      System.gc()
      val live = memory.getHeapMemoryUsage.getUsed / 1e6
      println(f"$corpus run $i%3d: $ms%8.1f ms, live heap $live%7.0f MB")
    }
    println(s"${compiler.outputCount} output files")
  }
}
