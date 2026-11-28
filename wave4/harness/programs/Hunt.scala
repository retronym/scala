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
  /** The unfixed base (upstream 2.13.x): decides whether a difference from 2.13.18 was fixed upstream or by an earlier wave. */
  lazy val baseC = Compiler("base", prop("base", ""), bridge + "/base")
  val inlineMaxChars = prop("inlineMaxChars", "15000").toInt
  /** -opt:local costs grow steeply with the handlers in a method (GE6); larger programs skip it. */
  val optMaxChars = prop("optMaxChars", "45000").toInt

  def ref = Config("ref", int)
  def configs(seed: Long, src: String): List[Config] =
    (if (src.length <= optMaxChars) List(Config("opt-local", int, List("-opt:local"))) else Nil) ::: List(
         Config("delambdafy-inline", int, List("-Ydelambdafy:inline")),
         Config("separate", int, separate = true),
         Config("v18", v18)) :::
    (if (seed % 20 == 10) List(Config("ycheck", int, List("-Ycheck:all"))) else Nil) :::
    (if (seed % 20 == 0 && src.length <= inlineMaxChars) List(Config("inline", int, List("-opt:inline:**", "-opt:local"))) else Nil)

  /** Known defects (wave4/known-signatures.md), excluded and counted. */
  val known = List("GE6" -> "LimitExceededException: Too many allocated bytes|^opt-local\\|ok/timeout-compile".r, "GE4" -> "found: x\\d*\\.type".r,
                   "WP1" -> "^ycheck\\|log\\|.*Tree has erroneous type: (N\\(\\)$|scala\\.[A-Za-z]+\\.box\\()".r,
                   "WP8" -> "^ycheck\\|log\\|.*Tree has erroneous type: ex\\.value\\(\\)$".r,
                   "WP9" -> "^ycheck\\|log\\|.*Tree has erroneous type: \\(head: [^,]*, next: List\\[".r,
                   "WP4" -> "^ycheck\\|log\\|.*Tree has erroneous type: N\\.this\\.N\\(\\)$".r, "WP2" -> "^ycheck\\|log\\|.*Tree has erroneous type: s\\.get\\(".r,
                   // -Ydelambdafy:inline: a closure nested in a closure is an inner class holding its enclosing closure (and that one's outer); serialization differs by design
                   "dl-ser" -> "^delambdafy-inline\\|diff\\|.*java\\.io\\.(NotSerializable|InvalidClass)Exception".r)
  def knownId(sig: String): Option[String] = known.collectFirst { case (id, rx) if rx.findFirstIn(sig).nonEmpty => id }
  val prePhases = Set("typer", "superaccessors", "extmethods", "pickler", "refchecks", "patmat")
  /** Known defects recognised from more than the signature: the configuration's whole log and the source. */
  def knownFull(c: Config, o: Outcome, src: String): Option[String] = {
    val phases = "during phase: (\\w+)".r.findAllMatchIn(o.log).map(_.group(1)).toSet
    if (c.name == "v18" && o.text.contains("Index 0 out of bounds for length 0")) Some("WP5") // fixed on the integration build
    else if (c.name == "opt-local" && o.kind == "crash" && o.text.contains("AnalyzerException") && src.contains("return ") && src.contains("finally")) Some("WP5")
    else if (c.name == "ycheck" && o.log.contains("erroneous type") && phases.nonEmpty && phases.subsetOf(prePhases) && src.contains("PartialFunction") && src.contains("return ")) Some("WP6")
    else None
  }

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
  def sigOf(units: List[String], c: Config): Option[String] = if (c.name == "ref") {
    val o = driver.eval(units, ref) // the reference alone: shrink a crash or a compile error of valid code
    if (o.kind == "ok" || o.kind == "run-throw" && o.text.contains("NonLocalReturn")) None else Some(s"ref|${o.kind}|" + normErr(o.text))
  } else {
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
    // subexpressions: hoist a nested group, drop a block statement or a case arm (Shrink)
    progress = true
    while (progress && left > 0) {
      progress = false
      for (u <- units.indices.reverse; j <- units(u).indices if left > 0) {
        var again = true
        while (again && left > 0) {
          again = false
          val line = units(u)(j)
          val it = Shrink.candidates(line)
          var found = false
          while (!found && it.hasNext && left > 0) {
            val c = it.next(); val x = units.updated(u, units(u).updated(j, c))
            if (ok(x)) { units = x; found = true; again = true; progress = true }
          }
        }
      }
    }
    units
  }
  def configByName(n: String): Config = n match {
    case "ycheck" => Config("ycheck", int, List("-Ycheck:all"))
    case "ref" => ref
    case "inline" => Config("inline", int, List("-opt:inline:**", "-opt:local"))
    case _ => configs(1, "").find(_.name == n).get
  }
  /** Shrinks a recorded program (units separated by //#unit) under one configuration while its signature holds. */
  def shrinkFile(file: String, cname: String, budget: Int): Unit = {
    driver = new Driver(new Pool(prop("threads", "6").toInt))
    val txt = new String(Files.readAllBytes(new File(file).toPath))
    val units = txt.split("\n//#unit\n").toList.map(_.linesIterator.filterNot(_.startsWith("// ")).toList)
    val c = configByName(cname)
    val sig = sigOf(units.map(_.mkString("\n")), c)
    println("SIG " + sig)
    sig.foreach { s =>
      val small = shrink(units, c, s, budget)
      val out = file.replaceAll("\\.scala$", "") + ".min.scala"
      Files.write(new File(out).toPath, (s"// $s\n" + small.map(_.mkString("\n")).mkString("\n//#unit\n") + "\n").getBytes)
      println("WROTE " + out + " " + small.map(_.map(_.length).sum).sum + " chars")
    }
    System.exit(0)
  }

  def main(args: Array[String]): Unit = {
    if (args(0) == "shrinkfile") shrinkFile(args(1), args(2), args.lift(3).fold(800)(_.toInt))
    val from = args(0).toLong; val secs = args(1).toInt; val outDir = new File(args(2)); outDir.mkdirs()
    val opts = args.drop(3).toSet
    val threads = prop("threads", "6").toInt; val progPar = prop("progPar", "2").toInt
    driver = new Driver(new Pool(threads))
    val t0 = System.nanoTime
    deadline = t0 + secs * 1000000000L
    val sigs = mutable.LinkedHashMap[String, (Int, Long)]()
    val knownHits = mutable.LinkedHashMap[String, Int]()
    val oracleRuns = mutable.LinkedHashMap[String, Int](); val oracleDiffs = mutable.LinkedHashMap[String, Int]()
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
      oracleRuns.synchronized { res.tail.foreach { case (c, o) =>
        oracleRuns(c.name) = oracleRuns.getOrElse(c.name, 0) + 1
        if (signature(base, c, o).nonEmpty) oracleDiffs(c.name) = oracleDiffs.getOrElse(c.name, 0) + 1 } }
      def countKnown(id: String) = knownHits.synchronized(knownHits(id) = knownHits.getOrElse(id, 0) + 1)
      val refKnown =
        if (base.kind == "compile-error" && (base.text.contains("Index 0 out of bounds for length 0") || base.text.startsWith("0: Error while emitting") && src.contains("return ") && src.contains("finally"))) Some("WP5")
        else if (base.kind == "crash" && base.text.contains("mergePrefixAndArgs(List(ErasedValueType")) Some("WP7")
        else if (base.kind == "compile-error" && base.text.contains("Sequence argument type annotation")) Some("WP10")
        else None
      refKnown.foreach(countKnown)
      val wp5 = refKnown.nonEmpty
      lazy val baseOut = driver.eval(units, Config("base", baseC))
      def same(a: Outcome, b: Outcome) = a.kind == b.kind && a.text == b.text
      for ((c, o) <- res.tail if !wp5; sig <- signature(base, c, o)) {
        val k = knownId(sig).orElse(knownFull(c, o, src)).orElse(
          if (c.name == "v18") Some(if (same(baseOut, base)) "v18-upstream" else if (same(baseOut, o)) "v18-wavefix" else null).filter(_ != null) else None)
        k match {
          case Some(id) => countKnown(id)
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
    println("ORACLES " + oracleRuns.map { case (k, v) => s"$k=${oracleDiffs.getOrElse(k, 0)}/$v" }.mkString(" "))
    errs.toList.sortBy(-_._2._1).foreach { case (k, (c, s)) => println(s"BASEFAIL $c first-seed=$s $k") }
    sigs.foreach { case (k, (c, s)) => println(s"SIG $c first-seed=$s $k") }
    Console.flush()
    System.exit(0)
  }
}
