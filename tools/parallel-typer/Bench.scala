// A warm-JVM measurement of -Yparallel-typer: compiles a corpus to the pickler repeatedly in one JVM, for each
// configuration, and prints the wall time of each run and of its typer phase.
//
//   Bench <args file> <warmup> <runs> <config>...
//
// A config is `serial`, `one` (the parallel path on one worker) or a number of threads, the last two with an optional
// suffix for the phases after the typer: `p` (a pool step each), `t` (the transforms fused), `f` (all fused), e.g.
// `onep`, `8p`, and `l` for the phases after the pickler a pool step each (with a full compile, -Dbench.full, which
// goes to class files in a temporary directory, deleted after each run): `4l` has the post-typer phases on the pool as well. The args file is the one
// the oracle writes (options and sources). Run with the quick compiler on the classpath, -Xss64m and enough heap.
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object Bench {
  def main(args: Array[String]): Unit = {
    val argsFile = args(0); val warmup = args(1).toInt; val runs = args(2).toInt
    val configs = args.drop(3).toList
    val full = System.getProperty("bench.full") != null
    val fileArgs = Files.readAllLines(Paths.get(argsFile)).asScala.toList.map(_.trim).filter(_.nonEmpty).map(_.stripPrefix("\"").stripSuffix("\""))
    for (config0 <- configs) {
      val config = config0
      val late = config0 != "serial" && config0.last == 'l'
      if (late) System.setProperty("scalac.parallelTyper.parallelLatePhases", "all") else System.clearProperty("scalac.parallelTyper.parallelLatePhases")
      val postTyper = if (late) Some("steps") else if (config0 == "serial") None else Map('p' -> "steps", 't' -> "fusedTransforms", 'f' -> "fused").get(config0.last)
      postTyper match {
        case Some(mode) => System.setProperty("scalac.parallelTyper.parallelPostTyper", mode)
        case None       => System.clearProperty("scalac.parallelTyper.parallelPostTyper")
      }
      val extra = (if (postTyper.isDefined) config0.init else config0) match {
        case "serial" => Nil
        case "one"    => System.setProperty("scalac.parallelTyper.oneThread", ""); List("-Yparallel-typer")
        case n        => System.clearProperty("scalac.parallelTyper.oneThread"); List("-Yparallel-typer", "-Yparallel-typer-threads", n)
      }
      val times = for (i <- 1 to warmup + runs) yield {
        val out = Files.createTempDirectory("bench")
        val s = new Settings(msg => sys.error(msg))
        s.usejavacp.value = true
        val (ok, rest) = s.processArguments((if (full) Nil else List("-Ystop-after:pickler")) ++ List("-d", out.toString) ++ extra ++ fileArgs, processAll = true)
        assert(ok, rest)
        val reporter = new StoreReporter(s)
        val g = new Global(s, reporter)
        val run = new g.Run
        val t0 = System.nanoTime()
        run.compile(rest)
        val t1 = System.nanoTime()
        val stream = Files.walk(out)
        try stream.iterator.asScala.toList.reverse.foreach(Files.delete) finally stream.close()
        val errors = reporter.infos.count(_.severity == reporter.ERROR)
        val total = (t1 - t0) / 1e6
        println(f"$config%-7s run $i%2d ${if (i <= warmup) "warmup" else "      "} total $total%8.1f ms  errors $errors")
        total
      }
      val measured = times.drop(warmup).sorted
      println(f"$config%-7s median ${measured(measured.length / 2)}%8.1f ms  min ${measured.head}%8.1f ms")
    }
  }
}
