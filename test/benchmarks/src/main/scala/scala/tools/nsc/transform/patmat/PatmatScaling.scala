package scala.tools.nsc.transform.patmat

/** Measures how the time in `patmat` grows with the number of cases, for each shape in [[PatmatShapes]].
 *
 *  {{{
 *  bench/Jmh/runMain scala.tools.nsc.transform.patmat.PatmatScaling [shapes] [maxN] [warmupSecs]
 *  bench/Jmh/runMain scala.tools.nsc.transform.patmat.PatmatScaling tup,ctor 1024
 *  }}}
 *
 *  For each shape, N doubles from 8 to `maxN`, stopping early when the next size is predicted to take
 *  over a minute. Each point is the minimum over a few compiles in one warmed-up compiler. The
 *  exponent `k` is the least-squares slope of log time against log N over the points of at least 5 ms,
 *  so time grows as N^k; `tail` is the slope over the last three points.
 *
 *  Sizes where the analysis gives up ("Cannot check match", past `AnalysisBudget.maxFormulaSize`)
 *  are reported but left out of the fit, as their time is not comparable.
 *
 *  This is a microbenchmark of one phase without forks or confidence intervals; it is meant for
 *  spotting a change in the exponent, which is far larger than the noise.
 */
object PatmatScaling {
  def main(args: Array[String]): Unit = {
    val shapes     = args.lift(0).fold(PatmatShapes.shapes)(_.split(',').toList)
    val maxN       = args.lift(1).fold(1024)(_.toInt)
    val warmupSecs = args.lift(2).fold(20)(_.toInt)
    val compiler   = new PatmatCompiler

    val warmEnd = System.nanoTime() + warmupSecs * 1000000000L
    while (System.nanoTime() < warmEnd) shapes.foreach(sh => compiler.patmatNanos(PatmatShapes.source(sh, 64)))

    for (shape <- shapes) {
      val points = collection.mutable.ListBuffer[(Int, Double)]()
      var n = 8
      var prevMs = -1d
      var continue = true
      while (continue && n <= maxN) {
        val (ms, gaveUp) = measure(compiler, PatmatShapes.source(shape, n))
        println(f"$shape%-6s N=$n%5d ${ms}%10.1f ms${if (gaveUp) "  (analysis gave up: not fitted)" else ""}")
        if (!gaveUp) points += ((n, ms))
        val growth = if (prevMs > 0) math.max(ms / prevMs, 2) else 4
        continue = ms * growth < 60000
        prevMs = ms
        n *= 2
      }
      println(f"$shape%-6s k=${fit(points.filter(_._2 >= 5).toList)}%s tail=${fit(points.takeRight(3).toList)}%s")
    }
  }

  /** Minimum over at least two compiles (more while under 3 s in total) of the phase time, and
   *  whether the analysis gave up. */
  private def measure(c: PatmatCompiler, code: String): (Double, Boolean) = {
    var best = Double.MaxValue
    var spent = 0d
    var reps = 0
    var gaveUp = false
    while (reps < 2 || (reps < 9 && spent < 3000)) {
      val (nanos, g) = c.compile(code)
      val ms = nanos / 1e6
      best = math.min(best, ms); spent += ms; reps += 1; gaveUp = g
    }
    (best, gaveUp)
  }

  private def fit(ps: List[(Int, Double)]): String =
    if (ps.lengthCompare(2) < 0) "-"
    else {
      val xs = ps.map(p => math.log(p._1.toDouble)); val ys = ps.map(p => math.log(p._2))
      val mx = xs.sum / xs.size; val my = ys.sum / ys.size
      val k = xs.zip(ys).map { case (x, y) => (x - mx) * (y - my) }.sum / xs.map(x => (x - mx) * (x - mx)).sum
      f"$k%.2f"
    }
}
