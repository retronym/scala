// A race finder for -Yparallel-typer: compiles a small corpus to the pickler over and over in one JVM, a fresh
// Global each time (so the classpath is loaded again), and prints the runs whose diagnostics differ from the first
// serial run's.
//
//   Stress <args file> <iterations> <threads> [extra scalac options...]
//
// Much faster than the oracle's process per run for a race in class loading; it does not compare pickles.
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object Stress {
  def main(args: Array[String]): Unit = {
    val argsFile = args(0); val iterations = args(1).toInt; val threads = args(2)
    val extraOpts = args.drop(3).toList
    val fileArgs = Files.readAllLines(Paths.get(argsFile)).asScala.toList.map(_.trim).filter(_.nonEmpty)
    val out = Files.createTempDirectory("stress").toString
    def compile(extra: List[String]): List[String] = {
      val s = new Settings(msg => sys.error(msg))
      s.usejavacp.value = true
      val (ok, rest) = s.processArguments(List("-Ystop-after:pickler", "-d", out) ++ extra ++ extraOpts ++ fileArgs, processAll = true)
      assert(ok, rest)
      val reporter = new StoreReporter(s)
      val g = new Global(s, reporter)
      try new g.Run().compile(rest)
      catch { case t: Throwable => return List("crash: " + t) ++ t.getStackTrace.take(30).map("  at " + _) }
      reporter.infos.toList.map(i => s"${i.severity} ${i.pos.source.file.name}:${if (i.pos.isDefined) i.pos.line else 0} ${i.msg}").sorted
    }
    val expected = compile(Nil)
    println(s"serial: ${expected.length} diagnostics")
    var failures = 0
    val t0 = System.nanoTime()
    for (i <- 1 to iterations) {
      val got = compile(List("-Yparallel-typer", "-Yparallel-typer-threads", threads))
      if (got != expected) {
        failures += 1
        println(s"run $i differs:")
        got.diff(expected).take(12).foreach(l => println("  + " + l))
        expected.diff(got).take(12).foreach(l => println("  - " + l))
      }
    }
    println(f"$failures of $iterations runs differ (${(System.nanoTime() - t0) / 1e9}%.1f s)")
    sys.exit(if (failures == 0) 0 else 1)
  }
}
