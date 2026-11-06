import scala.tools.nsc._
import scala.tools.nsc.reporters.StoreReporter
import java.nio.file._
import java.security.MessageDigest
import scala.jdk.CollectionConverters._
import scala.collection.mutable

object RoundBench {
  final case class Corpus(name: String, args: List[String], files: List[String])

  def out(tag: String): Path = { val d = Paths.get(sys.props("rb.out"), tag); clean(d); Files.createDirectories(d); d }
  def clean(d: Path): Unit = if (Files.exists(d)) { val s = Files.walk(d); try s.iterator.asScala.toList.reverse.foreach(Files.delete) finally s.close() }
  def newGlobal(c: Corpus, d: Path): Global = {
    val s = new Settings(m => Console.err.println(m))
    val cmd = new CompilerCommand(c.args ++ List("-d", d.toString), s)
    if (!cmd.ok) sys.error("bad arguments for " + c.name)
    new Global(cmd.settings, new StoreReporter(cmd.settings))
  }
  /** one Run: total ms and ms per phase (the first phase includes everything since t0, so `parser` carries the setup) */
  def run(g: Global, c: Corpus, t0: Long): (Double, List[(String, Double)]) = {
    val buf = mutable.ListBuffer[(String, Double)]()
    var last = t0
    val r = new g.Run {
      override def advancePhase(): Unit = { val n = System.nanoTime; buf += ((g.phase.name, (n - last) / 1e6)); last = n; super.advancePhase() }
    }
    r.compile(c.files)
    val end = System.nanoTime
    g.reporter match {
      case rr: StoreReporter if rr.hasErrors => println(c.name + " ERRORS: " + rr.infos.filter(_.severity == rr.ERROR).take(5).map(i => i.pos.toString.take(120) + " " + i.msg.take(200)).mkString("\n"))
      case _ =>
    }
    ((end - t0) / 1e6, buf.toList)
  }
  def digest(d: Path): Map[String, String] = {
    val s = Files.walk(d); try s.iterator.asScala.filter(Files.isRegularFile(_)).map { p =>
      d.relativize(p).toString -> MessageDigest.getInstance("SHA-1").digest(Files.readAllBytes(p)).map("%02x".format(_)).mkString }.toMap finally s.close()
  }
  def median(xs: Seq[Double]) = { val v = xs.sorted; v(v.size / 2) }
  def corpus(name: String, f: String): Corpus = {
    val ls = Files.readAllLines(Paths.get(f)).asScala.toList.filter(_.nonEmpty)
    val (srcs, opts) = ls.partition(l => l.endsWith(".scala") || l.endsWith(".java"))
    Corpus(name, opts, srcs)
  }
  def corpora(ss: Seq[String]) = ss.toList.map { s => val Array(n, f) = s.split("=", 2); corpus(n, f) }

  def roundMode(cs: List[Corpus], warm: Int, n: Int): Unit = cs.foreach { c =>
    val df = out("fresh-" + c.name); val dr = out("kept-" + c.name)
    val kept = newGlobal(c, dr)
    println(f"${c.name}: first run on the kept Global ${run(kept, c, System.nanoTime)._1}%.0f ms")
    val freshT, keptT = mutable.ArrayBuffer[Double]()
    val fp, rp = mutable.Map[String, mutable.ArrayBuffer[Double]]()
    for (i <- 0 until warm + n) {
      clean(df); Files.createDirectories(df)
      val t0 = System.nanoTime
      val g = newGlobal(c, df)
      val (ft, fps) = run(g, c, t0)
      g.close()
      clean(dr); Files.createDirectories(dr)
      val (rt, rps) = run(kept, c, System.nanoTime)
      if (i >= warm) {
        freshT += ft; keptT += rt
        fps.foreach { case (k, v) => fp.getOrElseUpdate(k, mutable.ArrayBuffer()) += v }
        rps.foreach { case (k, v) => rp.getOrElseUpdate(k, mutable.ArrayBuffer()) += v }
      }
      if (i == warm + n - 1 || i == 0) {
        val a = digest(df); val b = digest(dr)
        val ds = (a.keySet ++ b.keySet).filter(k => a.get(k) != b.get(k))
        println(s"${c.name} iter $i: fresh ${a.size} files, kept ${b.size} files, differ ${ds.size} ${ds.take(8).mkString(" ")}")
      }
    }
    println(f"${c.name}: fresh median ${median(freshT.toList)}%.0f ms, kept median ${median(keptT.toList)}%.0f ms ($n samples; ${c.files.size} files)")
    println("  phase ms (fresh / kept): " + fp.keys.toList.map(k => f"$k ${median(fp(k).toList)}%.0f/${rp.get(k).map(x => median(x.toList)).getOrElse(0d)}%.0f").mkString(", "))
    kept.close()
  }

  def phaseMode(cs: List[Corpus], warm: Int, n: Int, extra: List[String], label: String): Unit = {
    val rec = mutable.LinkedHashMap[String, mutable.Map[String, mutable.ArrayBuffer[Double]]]()
    val tot = mutable.Map[String, mutable.ArrayBuffer[Double]]()
    for (i <- 0 until warm + n; c <- cs) {
      val c1 = c.copy(args = extra ++ c.args)
      val d = out(label + "-" + c.name)
      val t0 = System.nanoTime
      val g = newGlobal(c1, d)
      Console.err.println(s"=== $label ${c.name} iter $i")
      val (t, ps) = run(g, c1, t0)
      g.close()
      if (i >= warm) {
        tot.getOrElseUpdate(c.name, mutable.ArrayBuffer()) += t
        val m = rec.getOrElseUpdate(c.name, mutable.Map())
        ps.foreach { case (k, v) => m.getOrElseUpdate(k, mutable.ArrayBuffer()) += v }
      }
      println(f"$label ${c.name} iter $i ${t}%.0f ms")
    }
    for ((c, m) <- rec) println(s"PHASES\t$label\t$c\t" + f"${median(tot(c).toList)}%.0f\t" + m.toList.map { case (k, v) => f"$k=${median(v.toList)}%.1f" }.mkString(" "))
  }

  def profileMode(c: Corpus, warm: Int, n: Int, lib: String, file: String, keep: Boolean): Unit = {
    val d0 = out("pr")
    val kept = if (keep) newGlobal(c, d0) else null
    def once() = { val d = out("pr"); val t0 = System.nanoTime; if (keep) run(kept, c, t0)._1 else { val g = newGlobal(c, d); val t = run(g, c, t0)._1; g.close(); t } }
    for (_ <- 0 until warm) once()
    val p = one.profiler.AsyncProfiler.getInstance(lib)
    p.execute("start,event=itimer,interval=1ms,jstackdepth=8192")
    val ts = (0 until n).map(_ => once())
    p.execute(s"stop,file=$file,collapsed")
    println(f"${c.name}: profiled $n runs, kept=$keep, median ${median(ts.toList)}%.0f ms -> $file")
  }

  def used(): Double = { for (_ <- 1 to 3) System.gc(); val r = Runtime.getRuntime; (r.totalMemory - r.freeMemory) / 1e6 }
  def retainMode(c: Corpus, runs: Int): Unit = {
    val d = out("ret"); val base = used()
    val g = newGlobal(c, d)
    for (i <- 1 to runs) { clean(d); Files.createDirectories(d); run(g, c, System.nanoTime); println(f"${c.name}: after run $i retained ${used() - base}%.0f MB") }
    g.close()
  }

  /** the corpora's sources and the first corpus's options in one Global; compare with the outputs of `phases` under the given label */
  def jointMode(cs: List[Corpus], warm: Int, n: Int, extra: List[String], label: String, opts: Corpus, compareLabel: String): Unit = {
    val c = Corpus("joint", extra ++ opts.args, cs.flatMap(_.files).distinct)
    val ts = mutable.ArrayBuffer[Double](); val m = mutable.Map[String, mutable.ArrayBuffer[Double]]()
    val d = out("joint-" + label)
    for (i <- 0 until warm + n) {
      clean(d); Files.createDirectories(d)
      val t0 = System.nanoTime
      val g = newGlobal(c, d); val (t, ps) = run(g, c, t0); g.close()
      println(f"joint $label iter $i ${t}%.0f ms")
      if (i >= warm) { ts += t; ps.foreach { case (k, v) => m.getOrElseUpdate(k, mutable.ArrayBuffer()) += v } }
    }
    println(s"PHASES\tjoint-$label\tjoint\t" + f"${median(ts.toList)}%.0f\t" + m.toList.map { case (k, v) => f"$k=${median(v.toList)}%.1f" }.mkString(" "))
    val j = digest(d)
    val sep = cs.flatMap(x => digest(Paths.get(sys.props("rb.out"), compareLabel + "-" + x.name)).toList).toMap
    val onlyJ = j.keySet -- sep.keySet; val onlyS = sep.keySet -- j.keySet; val diff = (j.keySet & sep.keySet).filter(k => j(k) != sep(k))
    println(s"joint ${j.size} files, separate ${sep.size} files, only joint ${onlyJ.size}, only separate ${onlyS.size}, differ ${diff.size}: ${diff.toList.sorted.take(15).mkString(" ")}")
  }

  def main(a: Array[String]): Unit = a(0) match {
    case "round" => roundMode(corpora(a.drop(3)), a(1).toInt, a(2).toInt)
    case "phases" => phaseMode(corpora(a.drop(5)), a(1).toInt, a(2).toInt, a(3).split(",").toList.filter(_.nonEmpty), a(4))
    case "retain" => retainMode(corpus(a(2), a(3)), a(1).toInt)
    case "profile" | "profile-kept" => profileMode(corpus(a(3), a(4)), a(1).toInt, a(2).toInt, a(5), a(6), a(0) == "profile-kept")
    case "joint" => jointMode(corpora(a.drop(7)), a(1).toInt, a(2).toInt, a(3).split(",").toList.filter(_.nonEmpty), a(4), corpus("o", a(5)), a(6))
  }
}
