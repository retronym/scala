package w4p

import java.io.File
import java.nio.file.Files
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.mutable
import w4d._

/** Generates programs and compares each configuration's outcome with the reference (integration build, default flags). */
object Hunt {
  def prop(k: String, d: String): String = sys.props.getOrElse("w4p." + k, d)
  val bridge = prop("bridge", "")
  lazy val int = Compiler("int", prop("int", ""), bridge + "/int")
  lazy val v18 = Compiler("v18", prop("v18", ""), bridge + "/v18")
  val inlineMaxChars = prop("inlineMaxChars", "15000").toInt

  def ref = Config("ref", int)
  def configs(seed: Long, src: String): List[Config] =
    List(Config("opt-local", int, List("-opt:local")),
         Config("delambdafy-inline", int, List("-Ydelambdafy:inline")),
         Config("separate", int, separate = true),
         Config("v18", v18)) :::
    (if (seed % 10 == 0) List(Config("ycheck", int, List("-Ycheck:all"))) else Nil) :::
    (if (seed % 20 == 0 && src.length <= inlineMaxChars) List(Config("inline", int, List("-opt:inline:**", "-opt:local"))) else Nil)

  /** Known defects (wave4/known-signatures.md), excluded and counted. */
  val known = List("GE6" -> "LimitExceededException: Too many allocated bytes".r, "GE4" -> "found: x\\d*\\.type".r)
  def knownId(sig: String): Option[String] = known.collectFirst { case (id, rx) if rx.findFirstIn(sig).nonEmpty => id }

  def normErr(e: String): String =
    e.linesIterator.nextOption().getOrElse("").replaceAll("^\\d+: ", "").replaceAll("[A-Za-z]+\\d+", "N").replaceAll("\\d+", "N").take(110)
  /** First line of what a compilation printed (-Ycheck complaints), without names and numbers. */
  def normLog(l: String): String = l.linesIterator.map(_.trim).filter(_.nonEmpty).take(2).mkString(" ").replaceAll("[A-Za-z]+\\d+", "N").replaceAll("\\d+", "N").take(110)
  def signature(base: Outcome, c: Config, o: Outcome): Option[String] =
    if (o.log.nonEmpty && base.log.isEmpty && base.compiled) Some(s"${c.name}|log|" + normLog(o.log))
    else if (base.kind == o.kind && base.text == o.text || (base.kind == "compile-error" && o.kind == "compile-error")) None
    else if (o.kind != "ok" || base.kind != "ok") Some(s"${c.name}|${base.kind}/${o.kind}|" + (if (o.kind == "ok") normErr(base.text) else normErr(o.text)))
    else {
      val (a, b) = (base.text.linesIterator.toList, o.text.linesIterator.toList)
      val d = a.zipAll(b, "", "").find { case (x, y) => x != y }.get
      def exc(s: String) = if (s.contains("!")) s.substring(s.indexOf('!') + 1).takeWhile(_ != ' ') else "value"
      Some(s"${c.name}|diff|${exc(d._1)}/${exc(d._2)}")
    }

  var driver: Driver = _
  var deadline = Long.MaxValue
  def sigOf(units: List[String], c: Config): Option[String] = {
    val Seq((_, b), (_, o)) = driver.evalAll(units, List(ref, c))
    if (b.kind == "compile-error" && !o.compiled) None else signature(b, c, o)
  }
  /** Delete units, then lines, while the signature is unchanged. */
  def shrink(units0: List[List[String]], c: Config, sig: String, budget: Int): List[List[String]] = {
    var units = units0; var left = budget
    def ok(x: List[List[String]]) = { left -= 1; if (System.nanoTime > deadline) left = 0; left >= 0 && sigOf(x.map(_.mkString("\n")), c).contains(sig) }
    var progress = true
    while (progress && left > 0) {
      progress = false
      var i = units.length - 2
      while (i >= 0 && left > 0) { val x = units.patch(i, Nil, 1); if (ok(x)) { units = x; progress = true }; i -= 1 }
      var u = units.length - 1
      while (u >= 0 && left > 0) {
        var j = units(u).length - 1
        while (j >= 1 && left > 0) {
          if (j < units(u).length) {
            val line = units(u)(j)
            if (line.startsWith("  ") && line.trim != "}" && !line.startsWith("  def run")) {
              val x = units.updated(u, units(u).patch(j, Nil, 1)); if (ok(x)) { units = x; progress = true }
            }
          }
          j -= 1
        }
        u -= 1
      }
    }
    units
  }

  def main(args: Array[String]): Unit = {
    val from = args(0).toLong; val secs = args(1).toInt; val outDir = new File(args(2)); outDir.mkdirs()
    val opts = args.drop(3).toSet
    val threads = prop("threads", "6").toInt; val progPar = prop("progPar", "2").toInt
    driver = new Driver(new Pool(threads))
    val t0 = System.nanoTime
    deadline = t0 + secs * 1000000000L
    val sigs = mutable.LinkedHashMap[String, (Int, Long)]()
    val knownHits = mutable.LinkedHashMap[String, Int]()
    val errs = mutable.LinkedHashMap[String, (Int, Long)]()
    val n = new AtomicInteger; val compiled = new AtomicInteger; val abandoned = new AtomicInteger; val baseThrow = new AtomicInteger
    val next = new AtomicLong(from)
    def worker(): Unit = while (System.nanoTime < deadline && driver.pool.stuck.get < 4) {
      val seed = next.getAndIncrement()
      val (us, k) = Gen.make(seed); abandoned.addAndGet(k)
      val units = us.map(_.mkString("\n")); val src = units.mkString("\n")
      n.incrementAndGet()
      val cs = configs(seed, src)
      val res = driver.evalAll(units, ref :: cs)
      val base = res.head._2
      if (!base.compiled) errs.synchronized {
        val key = base.kind + " " + normErr(base.text); errs(key) = (errs.get(key).fold(1)(_._1 + 1), errs.get(key).fold(seed)(_._2))
        Files.write(new File(outDir, s"basefail-$seed.scala").toPath, (units.mkString("\n//#unit\n") + "\n/* " + base.text + " */\n").getBytes)
      } else compiled.incrementAndGet()
      if (base.kind == "run-throw") baseThrow.incrementAndGet()
      for ((c, o) <- res.tail; sig <- signature(base, c, o)) {
        knownId(sig) match {
          case Some(id) => knownHits.synchronized(knownHits(id) = knownHits.getOrElse(id, 0) + 1)
          case None =>
            val first = sigs.synchronized { val f = !sigs.contains(sig); sigs(sig) = (sigs.get(sig).fold(1)(_._1 + 1), sigs.get(sig).fold(seed)(_._2)); f }
            if (first) {
              println(s"NEW $sig seed=$seed"); Console.flush()
              val small = if (opts("noshrink") || sig.contains("timeout")) us else shrink(us, c, sig, 150)
              Files.write(new File(outDir, s"sig-seed$seed-${c.name}.scala").toPath, (s"// $sig\n" + small.map(_.mkString("\n")).mkString("\n//#unit\n") + "\n").getBytes)
            }
        }
      }
      if (opts("dump")) Files.write(new File(outDir, s"out-$seed.txt").toPath, res.map { case (c, o) => s"== ${c.name} ${o.kind}\n${o.text}" }.mkString("\n").getBytes)
    }
    val ws = (1 to progPar).map(_ => new Thread(null, () => worker(), "prog", 64L << 20))
    ws.foreach(_.start()); ws.foreach(_.join())
    val mins = (System.nanoTime - t0) / 6e10
    val N = n.get
    println(f"==== programs=$N compiled=${compiled.get} (${100.0 * compiled.get / N}%.1f%%) ref-run-throw=${baseThrow.get} generator-abandoned=${abandoned.get} compiles=${driver.compiles.get} stuck=${driver.pool.stuck.get} programs/min=${N / mins}%.1f seeds=$from..${next.get - 1}")
    println("TIMES " + driver.timesReport)
    println("KNOWN " + knownHits.mkString(" "))
    errs.toList.sortBy(-_._2._1).foreach { case (k, (c, s)) => println(s"BASEFAIL $c first-seed=$s $k") }
    sigs.foreach { case (k, (c, s)) => println(s"SIG $c first-seed=$s $k") }
    Console.flush()
    System.exit(0)
  }
}
