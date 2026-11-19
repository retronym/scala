// The x-ray's recorder: compiles a corpus to the pickler in one warm JVM, then once per configuration with the
// timeline on (`ParallelTyperTimeline`), and writes each recording as JSON and all of them as `timeline.data.js`.
//
//   XrayBench <args file> <out dir> <corpus name> <warmup> <config>...
//   XrayBench --overhead <args file> <config> <warmup> <pairs>
//   XrayBench --rounds <args file> <out dir> <corpus name> <warmup> <rounds> <config>...
//
// A config is `serial` or a number of typer threads. The warm-up cycles through the configs, unrecorded. With
// --overhead, the same config alternately with and without the timeline, and the medians of each. With --rounds, the
// configs in turn, each recorded `rounds` times (run-<config>-<i>.json), for `rounds.py` to compare step by step.
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.ParallelTyperTimeline
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object XrayBench {
  def main(args: Array[String]): Unit =
    if (args(0) == "--overhead") overhead(args(1), args(2), args(3).toInt, args(4).toInt)
    else if (args(0) == "--rounds") rounds(args(1), Paths.get(args(2)), args(3), args(4).toInt, args(5).toInt, args.drop(6).toList)
    else record(args(0), Paths.get(args(1)), args(2), args(3).toInt, args.drop(4).toList)

  def fileArgs(argsFile: String): List[String] =
    Files.readAllLines(Paths.get(argsFile)).asScala.toList.map(_.trim).filter(_.nonEmpty).map(_.stripPrefix("\"").stripSuffix("\""))

  def options(config: String): List[String] = config match {
    case "serial" => Nil
    case n        => List("-Yparallel-typer", "-Yparallel-typer-threads", n.toInt.toString)
  }

  // -Dxray.full: every phase, to class files in a temporary directory
  private val stopOptions = if (System.getProperty("xray.full") != null) Nil else List("-Ystop-after:pickler")

  // -Dxray.disk: the classpath and the sources from the file system and the output to it, as scalac from a shell.
  // By default nothing is on disk during a compilation (XrayMemory.scala).
  private val onDisk = System.getProperty("xray.disk") != null

  /** One compilation; its wall time in ms and its error count. */
  def compile(config: String, files: List[String]): (Double, Int) =
    if (onDisk) compileOnDisk(config, files)
    else {
      val (g, reporter, sources) = scala.tools.nsc.xray.InMemory.compiler(stopOptions ++ options(config) ++ files)
      val run = new g.Run
      val t0 = System.nanoTime()
      run.compileSources(sources)
      val t1 = System.nanoTime()
      val errors = reporter.infos.filter(_.severity == reporter.ERROR)
      // a corpus that does not compile is not a recording of a compilation: say why, once
      if (errors.nonEmpty && !saidErrors) { saidErrors = true; errors.take(3).foreach(e => Console.err.println(s"error: ${e.pos.source.file.name}:${e.pos.line}: ${e.msg}")) }
      ((t1 - t0) / 1e6, errors.size)
    }
  private var saidErrors = false

  private def compileOnDisk(config: String, files: List[String]): (Double, Int) = {
    val s = new Settings(msg => sys.error(msg))
    s.usejavacp.value = true
    val out = Files.createTempDirectory("xray")
    val (ok, rest) = s.processArguments(stopOptions ++ List("-d", out.toString) ++ options(config) ++ files, processAll = true)
    assert(ok, rest)
    val reporter = new StoreReporter(s)
    val g = new Global(s, reporter)
    val run = new g.Run
    val t0 = System.nanoTime()
    run.compile(rest)
    val t1 = System.nanoTime()
    delete(out)
    ((t1 - t0) / 1e6, reporter.infos.count(_.severity == reporter.ERROR))
  }

  private def delete(p: Path): Unit = {
    if (Files.isDirectory(p)) Files.list(p).iterator.asScala.foreach(delete)
    Files.deleteIfExists(p)
  }

  def warm(configs: List[String], files: List[String], warmup: Int): Unit =
    for (i <- 1 to warmup) {
      val c = configs((i - 1) % configs.length)
      val (ms, errors) = compile(c, files)
      println(f"warm-up $i%2d $c%-6s $ms%8.1f ms${if (errors > 0) s"  $errors errors" else ""}")
    }

  def record(argsFile: String, out: Path, corpus: String, warmup: Int, configs: List[String]): Unit = {
    val files = fileArgs(argsFile)
    warm(configs, files, warmup)
    Files.createDirectories(out)
    val jsons = for (c <- configs) yield recordOne(out, corpus, warmup, files, c, s"run-$c.json")
    Files.write(out.resolve("timeline.data.js"), jsons.mkString("window.XRAY_RUNS = (window.XRAY_RUNS || []).concat([\n", ",\n", "]);\n").getBytes(UTF_8))
  }

  private def recordOne(out: Path, corpus: String, warmup: Int, files: List[String], c: String, fileName: String): String = {
    System.gc()
    val tl = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
    tl.meta.put("corpus", corpus)
    tl.meta.put("config", c)
    tl.meta.put("threads", if (c == "serial") "0" else c)
    tl.meta.put("flags", (stopOptions ++ options(c) ++ files.filter(_.startsWith("-"))).mkString(" "))
    tl.meta.put("sources", files.count(f => f.endsWith(".scala") || f.endsWith(".java")).toString)
    tl.meta.put("warmup", warmup.toString)
    val threads = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val allocated0 = threads.getTotalThreadAllocatedBytes
    ParallelTyperTimeline.current = tl
    val (ms, errors) = try compile(c, files) finally ParallelTyperTimeline.current = null
    tl.meta.put("allocatedMB", ((threads.getTotalThreadAllocatedBytes - allocated0) / 1000000).toString)
    tl.meta.put("compileMs", f"$ms%.1f")
    tl.meta.put("errors", errors.toString)
    val json = tl.toJson
    Files.write(out.resolve(fileName), json.getBytes(UTF_8))
    println(f"recorded $fileName%-20s $ms%8.1f ms${if (errors > 0) s"  $errors errors" else ""}")
    json
  }

  def rounds(argsFile: String, out: Path, corpus: String, warmup: Int, rounds: Int, configs: List[String]): Unit = {
    val files = fileArgs(argsFile)
    warm(configs, files, warmup)
    Files.createDirectories(out)
    for (i <- 1 to rounds; c <- configs) recordOne(out, corpus, warmup, files, c, s"run-$c-$i.json")
  }

  def overhead(argsFile: String, config: String, warmup: Int, pairs: Int): Unit = {
    val files = fileArgs(argsFile)
    warm(List(config), files, warmup)
    val off, on = collection.mutable.ArrayBuffer.empty[Double]
    for (i <- 1 to pairs) {
      off += compile(config, files)._1
      ParallelTyperTimeline.current = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
      try on += compile(config, files)._1 finally ParallelTyperTimeline.current = null
      println(f"pair $i%2d off ${off.last}%8.1f ms  on ${on.last}%8.1f ms")
    }
    def median(xs: collection.Seq[Double]) = { val s = xs.sorted; s(s.length / 2) }
    println(f"$config: timeline off median ${median(off)}%.1f ms (min ${off.min}%.1f), on median ${median(on)}%.1f ms (min ${on.min}%.1f): ${100 * (median(on) / median(off) - 1)}%+.1f%%")
  }
}
