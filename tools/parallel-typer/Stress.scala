// A race finder for -Yparallel-typer: compiles a small corpus to the pickler over and over in one JVM, a fresh
// Global each time (so the classpath is loaded again), and prints the runs whose diagnostics differ from those of
// a parallel run on one thread.
//
//   Stress <args file> <iterations> <threads> [extra scalac options...]
//
// Much faster than the oracle's process per run for a race in class loading; it does not compare pickles.
// With -Dstress.full, every phase runs and the class files, are compared too (by checksum).
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.io.AbstractFile

object Stress {
  def main(args: Array[String]): Unit = {
    val argsFile = args(0); val iterations = args(1).toInt; val threads = args(2)
    val extraOpts = args.drop(3).toList
    val fileArgs = Files.readAllLines(Paths.get(argsFile)).asScala.toList.map(_.trim).filter(_.nonEmpty).map(_.stripPrefix("\"").stripSuffix("\""))
    val out = Files.createTempDirectory("stress").toString
    val full = System.getProperty("stress.full") != null
    def deleteAll(p: java.nio.file.Path): Unit = {
      val stream = Files.walk(p)
      try stream.iterator.asScala.toList.reverse.foreach(Files.delete) finally stream.close()
    }
    def checksums(dir: AbstractFile): List[String] = dir.iterator.toList.flatMap { f =>
      if (f.isDirectory) checksums(f)
      else { val c = new java.util.zip.CRC32; c.update(f.toByteArray); List(s"class ${f.path} ${c.getValue}") }
    }
    def compile(extra: List[String]): List[String] = {
      val s = new Settings(msg => sys.error(msg))
      s.usejavacp.value = true
      val (ok, rest) = s.processArguments((if (full) Nil else List("-Ystop-after:pickler")) ++ List("-d", out) ++ extra ++ extraOpts ++ fileArgs, processAll = true)
      assert(ok, rest)
      // a directory, not memory: a virtual directory makes the back end write in the order of the units
      val classesDir = Files.createTempDirectory(Paths.get(out), "c")
      val classes = AbstractFile.getDirectory(classesDir.toFile)
      if (full) s.outputDirs.setSingleOutput(classes)
      val reporter = new StoreReporter(s)
      val g = new Global(s, reporter)
      try new g.Run().compile(rest)
      catch { case t: Throwable => return List("crash: " + t) ++ t.getStackTrace.take(30).map("  at " + _) }
      val sums = if (full) checksums(classes).map(_.replace(classesDir.toString, "")) else Nil
      deleteAll(classesDir)
      (reporter.infos.toList.map(i => s"${i.severity} ${i.pos.source.file.name}:${if (i.pos.isDefined) i.pos.line else 0} ${i.msg}") ++ sums).sorted
    }
    // the reference is the parallel path on one thread, as in the oracle: it differs from serial by order effects
    // (which occurrence of a feature warning carries the explanation)
    val serial = compile(Nil)
    val expected = compile(List("-Yparallel-typer", "-Yparallel-typer-threads", "1"))
    println(s"serial: ${serial.length} diagnostics; the one-thread run ${if (serial == expected) "agrees" else "differs"}")
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
