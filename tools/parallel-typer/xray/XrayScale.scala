// What the machine allows N independent compilations, with the numbers to say where the shortfall goes. Driven by
// scale.py, which reads the collector's log for the windows these lines report.
//
//   XrayScale <args file> <warmup> <rounds> <token>...   tokens in turn, `rounds` times, in one JVM
//   XrayScale --child <args file> <warmup>               one compilation per `go` on stdin; `gc` collects first
//   XrayScale --spin <ms> <rounds> <k>...                no compiler: how fast k threads run a small loop and a walk over memory
//
// A token is k<N> (N serial compilations at once, each with its own Global) or p<N> (one compilation on N typer threads).
// Each result is one `R {json}` line. With -Dxray.asprof=<dir>,<options> (and async-profiler's jar on the classpath), a
// recording of each batch at <dir>/<token>-<round>.jfr.
import java.lang.management.ManagementFactory

object XrayScale {
  private val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private val os = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
  // since JDK 26
  private val gcCpu: () => Long = {
    val mem = ManagementFactory.getMemoryMXBean
    try { val m = classOf[java.lang.management.MemoryMXBean].getMethod("getTotalGcCpuTime"); () => m.invoke(mem).asInstanceOf[java.lang.Long].longValue }
    catch { case _: NoSuchMethodException => () => -1L }
  }
  private def arr(xs: Seq[Double]) = xs.map(x => f"$x%.1f").mkString("[", ",", "]")

  private final case class One(wall: Double, cpu: Double, alloc: Long, errors: Int)
  private def compileHere(config: String, files: List[String]): One = {
    val id = Thread.currentThread.getId
    val c0 = threads.getThreadCpuTime(id); val a0 = threads.getThreadAllocatedBytes(id)
    val (ms, errors) = XrayBench.compile(config, files)
    One(ms, (threads.getThreadCpuTime(id) - c0) / 1e6, threads.getThreadAllocatedBytes(id) - a0, errors)
  }

  private def result(token: String, round: Int, ones: Seq[One], batch: Double, t0: Long, t1: Long, nanos: Long, proc0: Long, gc0: Long, totalAlloc: Long): Unit =
    println(s"""R {"token":"$token","round":$round,"batch":${f"$batch%.1f"},"walls":${arr(ones.map(_.wall))},"cpus":${arr(ones.map(_.cpu))},""" +
      s""""allocs":${ones.map(_.alloc).mkString("[", ",", "]")},"totalAlloc":$totalAlloc,"errors":${ones.map(_.errors).sum},"procCpu":${f"${(os.getProcessCpuTime - proc0) / 1e6}%.1f"},""" +
      s""""gcCpu":${f"${(gcCpu() - gc0) / 1e6}%.1f"},"t0":$t0,"t1":$t1,"nanoMs":${nanos / 1000000},"load":${f"${os.getSystemLoadAverage}%.2f"}}""")

  private val asprof = Option(System.getProperty("xray.asprof")).map(_.split(",", 2))
  private def profiler(command: String): Unit = {
    val c = Class.forName("one.profiler.AsyncProfiler")
    c.getMethod("execute", classOf[String]).invoke(c.getMethod("getInstance").invoke(null), command)
    ()
  }

  private def batch(token: String, round: Int, files: List[String]): Unit = {
    System.gc()
    asprof.foreach { case Array(dir, options) => profiler(s"start,jfr,$options,file=$dir/$token-$round.jfr") }
    val (k, config) = if (token(0) == 'k') (token.tail.toInt, "serial") else (1, token.tail)
    val res = new java.util.concurrent.ConcurrentLinkedQueue[One]
    val ts = (1 to k).map(i => new Thread(null, () => res.add(compileHere(config, files)), s"indep-$i", 1L << 28))
    val proc0 = os.getProcessCpuTime; val gc0 = gcCpu(); val alloc0 = threads.getTotalThreadAllocatedBytes
    val t0 = System.currentTimeMillis; val n0 = System.nanoTime
    ts.foreach(_.start()); ts.foreach(_.join())
    val n1 = System.nanoTime; val t1 = System.currentTimeMillis
    asprof.foreach(_ => profiler("stop"))
    result(token, round, res.toArray(Array.empty[One]).toSeq, (n1 - n0) / 1e6, t0, t1, n1 - n0, proc0, gc0, threads.getTotalThreadAllocatedBytes - alloc0)
  }

  // a compile thread's stack, as `batch` has it
  private def onBigStack[T](body: => T): T = {
    var r: Option[T] = None
    val t = new Thread(null, () => r = Some(body), "warm", 1L << 28); t.start(); t.join(); r.get
  }

  private def warm(files: List[String], warmup: Int, config: Int => String): Unit =
    for (i <- 1 to warmup) {
      val c = config(i)
      val (ms, errors) = onBigStack(XrayBench.compile(c, files))
      println(f"warm-up $i%2d $c%-6s $ms%8.1f ms${if (errors > 0) s"  $errors errors" else ""}")
    }

  private def child(argsFile: String, warmup: Int): Unit = {
    val files = XrayBench.fileArgs(argsFile)
    warm(files, warmup, _ => "serial")
    println("ready")
    val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    var round = 0
    Iterator.continually(in.readLine()).takeWhile(_ != null).foreach {
      case "gc" => System.gc(); println("collected")
      case "go" =>
        round += 1
        val proc0 = os.getProcessCpuTime; val gc0 = gcCpu(); val alloc0 = threads.getTotalThreadAllocatedBytes
        val t0 = System.currentTimeMillis; val n0 = System.nanoTime
        val one = onBigStack(compileHere("serial", files))
        val n1 = System.nanoTime; val t1 = System.currentTimeMillis
        result("child", round, Seq(one), (n1 - n0) / 1e6, t0, t1, n1 - n0, proc0, gc0, threads.getTotalThreadAllocatedBytes - alloc0)
    }
  }

  // Xorshift in registers: the core's speed and nothing else.
  private def core(until: Long): Long = {
    var x = 88172645463325252L; var n = 0L
    while (System.nanoTime < until) {
      var i = 0
      while (i < 100000) { x ^= x << 13; x ^= x >>> 7; x ^= x << 17; i += 1 }
      n += 100000
    }
    if (x == 0) -1 else n
  }
  // A walk over a random cycle far larger than any cache: a load's latency under the others' traffic.
  private def cycle(size: Int, seed: Int): Array[Int] = {
    val perm = Array.range(0, size); val r = new java.util.SplittableRandom(seed)
    var i = size - 1
    while (i > 0) { val j = r.nextInt(i + 1); val t = perm(i); perm(i) = perm(j); perm(j) = t; i -= 1 }
    val next = new Array[Int](size)
    i = 0
    while (i < size) { next(perm(i)) = perm((i + 1) % size); i += 1 }
    next
  }
  private def walk(next: Array[Int], until: Long): Long = {
    var p = 0; var n = 0L
    while (System.nanoTime < until) {
      var i = 0
      while (i < 10000) { p = next(p); i += 1 }
      n += 10000
    }
    if (p < -1) -1 else n
  }

  private def spin(ms: Int, rounds: Int, ks: List[Int]): Unit = {
    val maxK = ks.max
    val arrays = (1 to maxK).map(i => cycle(1 << 26, i)) // 256 MB each
    def run(k: Int, body: (Int, Long) => Long): Seq[Double] = {
      val rates = new java.util.concurrent.ConcurrentLinkedQueue[java.lang.Double]
      val until = System.nanoTime + ms * 1000000L
      val ts = (0 until k).map(i => new Thread(() => { rates.add(java.lang.Double.valueOf(body(i, until).toDouble / ms)); () }))
      ts.foreach(_.start()); ts.foreach(_.join())
      rates.toArray(Array.empty[java.lang.Double]).toSeq.map(_.doubleValue)
    }
    val coreBody: (Int, Long) => Long = (_, u) => core(u)
    val walkBody: (Int, Long) => Long = (i, u) => walk(arrays(i), u)
    for (_ <- 1 to 3) { run(1, coreBody); run(1, walkBody) }
    for (r <- 1 to rounds; k <- ks; (name, body) <- List(("core", coreBody), ("walk", walkBody)))
      println(s"""R {"kind":"$name","k":$k,"round":$r,"rates":${arr(run(k, body))}}""")
  }

  def main(args: Array[String]): Unit =
    args(0) match {
      case "--child" => child(args(1), args(2).toInt)
      case "--spin"  => spin(args(1).toInt, args(2).toInt, args.drop(3).map(_.toInt).toList)
      case _ =>
        val files = XrayBench.fileArgs(args(0))
        val tokens = args.drop(3).toList
        // the configurations to run are the configurations to warm up
        val configs = tokens.map(t => if (t(0) == 'k') "serial" else t.tail).distinct
        warm(files, args(1).toInt, i => configs((i - 1) % configs.length))
        println("ready")
        for (r <- 1 to args(2).toInt; t <- tokens) batch(t, r, files)
    }
}
