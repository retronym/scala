// The x-ray's profile runs: like XrayBench, with an async-profiler JFR recording around each recorded compilation, for
// `profile.sh`. Needs `-agentpath:<patched libasyncProfiler>` and, on the classpath, its async-profiler.jar and
// jfr-converter.jar. Keeps `run-<config>.jfr` and adds what the page needs to the run's JSON as `profile`.
//
// CPU and wall clock are a compilation each: on macOS `event=cpu` is the wall clock engine sampling running threads,
// which cannot run beside `wall`, and `event=itimer`, which can, delivers its signal at the next kernel trap, so that
// a tenth of the samples land in whichever syscalls the x-ray itself makes.
//
//   XrayProfile <args file> <out dir> <corpus name> <warmup> <repeats> <cpu interval> <wall interval> <config>...
//   XrayProfile --overhead <args file> <config> <warmup> <pairs> <cpu interval> <wall interval> <out dir>
//
// One compilation has too few samples to say much about a step or a unit, so a profiled config is `repeats` compilations
// under one JFR recording. The run shown is the one of median length. Each sample of another compilation is projected
// onto it: from the span around it to the span of that step and unit, proportionally, on that span's thread. Filtering
// by step, unit or span is then exact, and a window of time means the same work in every compilation.
//
// A config is `<threads>` (CPU profile), `<threads>-wall` (wall clock profile), or `serial`, which is recorded without
// a profile as the baseline of the speed-ups. With --overhead, the same config in turn with nothing on, the timeline,
// and the timeline with each profile.
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import scala.reflect.internal.util.ParallelTyperTimeline
import one.convert.{Arguments, JfrConverter}
import one.jfr.JfrReader
import one.jfr.event.{ExecutionSample, SpanEvent}
import one.profiler.AsyncProfiler

object XrayProfile {
  def main(args: Array[String]): Unit =
    if (args(0) == "--overhead") overhead(args(1), args(2), args(3).toInt, args(4).toInt, args(5), args(6), Paths.get(args(7)))
    else if (args(0) == "--frames") frames(args(1), Paths.get(args(2)), args(3).toInt, args(4).toInt, args(5).toInt, args(6), args.drop(7).toList)
    else record(args(0), Paths.get(args(1)), args(2), args(3).toInt, args(4).toInt, args(5), args(6), args.drop(7).toList)

  /** As perflab profiles: native frames through the VM, and a stack depth the typer's recursion does not exceed. */
  private def startProfiler(event: String, interval: String, jfr: Path): Unit =
    AsyncProfiler.getInstance().execute(s"start,jfr,event=$event,interval=$interval,jstackdepth=8192,cstack=vm,features=mixed+vtable+comptask,file=$jfr")
  private def stopProfiler(): Unit = AsyncProfiler.getInstance().execute("stop")

  /** For a comparison by frame (`frames.py`): the configs in turn, `rounds` times, a CPU profile of `repeats` compilations
   *  each (`cmp-<config>-<round>.jfr`). Unlike `record`, `serial` is profiled too. */
  def frames(argsFile: String, out: Path, warmup: Int, rounds: Int, repeats: Int, interval: String, configs: List[String]): Unit = {
    val files = XrayBench.fileArgs(argsFile)
    Files.createDirectories(out)
    XrayBench.warm(configs, files, warmup)
    for (r <- 1 to rounds; c <- configs) {
      System.gc()
      startProfiler("cpu", interval, out.resolve(s"cmp-$c-$r.jfr"))
      val mss = for (_ <- 1 to repeats) yield {
        ParallelTyperTimeline.current = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
        try XrayBench.compile(c, files)._1 finally ParallelTyperTimeline.current = null
      }
      stopProfiler()
      println(s"profiled $c round $r: ${mss.map(ms => f"$ms%.0f").mkString(" ")} ms")
    }
  }

  def record(argsFile: String, out: Path, corpus: String, warmup: Int, repeats: Int, cpu: String, wall: String, configs: List[String]): Unit = {
    val files = XrayBench.fileArgs(argsFile)
    Files.createDirectories(out)
    val compiled = configs.map(_.stripSuffix("-wall")).distinct
    XrayBench.warm(compiled, files, warmup)
    val jsons = for (c <- configs) yield {
      System.gc()
      val profiled = c != "serial"
      val isWall = c.endsWith("-wall")
      val threads = c.stripSuffix("-wall")
      val jfr = out.resolve(s"run-$c.jfr")
      if (isWall) startProfiler("wall", wall, jfr)
      else if (profiled) startProfiler("cpu", cpu, jfr)
      // (timeline, ms, errors) of each compilation
      val compilations = for (_ <- 1 to (if (profiled) repeats else 1)) yield {
        val tl = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
        ParallelTyperTimeline.current = tl
        val (ms, errors) = try XrayBench.compile(threads, files) finally ParallelTyperTimeline.current = null
        (tl, ms, errors)
      }
      if (profiled) stopProfiler()
      val (tl, ms, errors) = compilations.sortBy(_._2).apply(compilations.length / 2)
      tl.meta.put("corpus", corpus)
      tl.meta.put("config", c)
      tl.meta.put("threads", if (c == "serial") "0" else threads)
      tl.meta.put("flags", ("-Ystop-after:pickler" :: XrayBench.options(threads) ++ files.filter(_.startsWith("-"))).mkString(" "))
      tl.meta.put("sources", files.count(f => f.endsWith(".scala") || f.endsWith(".java")).toString)
      tl.meta.put("warmup", warmup.toString)
      if (profiled) {
        tl.meta.put("profiled", if (isWall) s"wall $wall" else s"cpu $cpu")
        tl.meta.put("repeats", compilations.map(x => f"${x._2}%.0f").mkString("median of ", " ", " ms"))
      }
      tl.meta.put("compileMs", f"$ms%.1f")
      tl.meta.put("errors", errors.toString)
      val base = tl.toJson
      val json =
        if (!profiled) base
        else {
          val profile = ProfileData.read(jfr, compilations.map(_._1).toList, tl, corpus, c, if (isWall) wall else cpu)
          base.substring(0, base.lastIndexOf('}')) + ",\n\"profile\":" + profile + "}\n"
        }
      Files.write(out.resolve(s"run-$c.json"), json.getBytes(UTF_8))
      println(f"recorded $c%-6s $ms%8.1f ms${if (errors > 0) s"  $errors errors" else ""}  ${json.length >> 10} KB")
      json
    }
    Files.write(out.resolve("timeline.data.js"), jsons.mkString("window.XRAY_RUNS = (window.XRAY_RUNS || []).concat([\n", ",\n", "]);\n").getBytes(UTF_8))
  }

  def overhead(argsFile: String, config: String, warmup: Int, pairs: Int, cpu: String, wall: String, out: Path): Unit = {
    val files = XrayBench.fileArgs(argsFile)
    Files.createDirectories(out)
    XrayBench.warm(List(config), files, warmup)
    val off, timeline, cpuProfiled, wallProfiled = mutable.ArrayBuffer.empty[Double]
    val jfr = out.resolve("overhead.jfr")
    for (i <- 1 to pairs) {
      off += XrayBench.compile(config, files)._1
      ParallelTyperTimeline.current = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
      try timeline += XrayBench.compile(config, files)._1 finally ParallelTyperTimeline.current = null
      def profiled(event: String, interval: String, into: mutable.ArrayBuffer[Double]): Unit = {
        startProfiler(event, interval, jfr)
        ParallelTyperTimeline.current = new ParallelTyperTimeline(ParallelTyperTimeline.threshold)
        try into += XrayBench.compile(config, files)._1 finally { ParallelTyperTimeline.current = null; stopProfiler() }
      }
      profiled("cpu", cpu, cpuProfiled)
      profiled("wall", wall, wallProfiled)
      println(f"round $i%2d off ${off.last}%8.1f  timeline ${timeline.last}%8.1f  cpu ${cpuProfiled.last}%8.1f  wall ${wallProfiled.last}%8.1f ms")
    }
    Files.deleteIfExists(jfr)
    def median(xs: collection.Seq[Double]) = { val s = xs.sorted; s(s.length / 2) }
    def line(n: String, xs: collection.Seq[Double]) = f"$n%-9s median ${median(xs)}%8.1f min ${xs.min}%8.1f ms  ${100 * (median(xs) / median(off) - 1)}%+.1f%%"
    println(s"$config:\n${line("off", off)}\n${line("timeline", timeline)}\n${line("cpu", cpuProfiled)}\n${line("wall", wallProfiled)}")
  }
}

/** The samples of a JFR recording as the page's `profile` object; the format is in the README. */
object ProfileData {
  private final class Trie {
    val frames = mutable.ArrayBuffer.empty[String]
    private val frameIds = new java.util.HashMap[String, Integer]()
    val parent = mutable.ArrayBuilder.make[Int]
    val frame = mutable.ArrayBuilder.make[Int]
    var nodes = 0
    private val children = new java.util.HashMap[java.lang.Long, Integer]()
    def frameId(name: String): Int = {
      val i = frameIds.get(name)
      if (i ne null) i.intValue else { frames += name; frameIds.put(name, frames.length - 1); frames.length - 1 }
    }
    def child(p: Int, f: Int): Int = {
      val k = java.lang.Long.valueOf((p.toLong << 32) | f.toLong)
      val i = children.get(k)
      if (i ne null) i.intValue
      else { parent += p; frame += f; children.put(k, nodes); nodes += 1; nodes - 1 }
    }
  }

  /** The frames of generated forwarders and of the closures' plumbing, which say nothing about where the time went. */
  private def isNoise(name: String): Boolean =
    name.endsWith("$adapted") || name.startsWith("scala.runtime.java8.") || name.contains("$$Lambda") || name.startsWith("java.lang.invoke.LambdaForm")

  /** Compact names: the compiler's and the JDK's frames lose their package. */
  private def short(name: String): String = {
    val n = name.replace('/', '.')
    val stripped =
      if (n.startsWith("scala.tools.nsc.")) n.substring(16)
      else if (n.startsWith("scala.reflect.internal.")) n.substring(23)
      else n
    stripped
  }

  private val jvmFrame = java.util.regex.Pattern.compile("^(_|\\[|[A-Z][A-Za-z0-9_]*::)")

  /** The profile of `compilations` (in order, under one JFR), projected onto `shown`, one of them. */
  def read(jfrPath: Path, compilations: List[ParallelTyperTimeline], shown: ParallelTyperTimeline, corpus: String, config: String, interval: String): String = {
    val jfr = new JfrReader(jfrPath.toString)
    try {
      val names = new JfrConverter(jfr, new Arguments()) { methodNames = new one.jfr.Dictionary[String]() }
      val perTick = 1e6 / jfr.ticksPerSec
      val tls = compilations.toArray
      val starts = tls.map(_.profilerStart)
      /** The compilation running at this time, or -1 before the first. */
      def compilationAt(ticks: Long): Int = {
        val i = java.util.Arrays.binarySearch(starts, ticks)
        if (i >= 0) i else -i - 2
      }
      def us(c: Int, ticks: Long): Long = ((ticks - starts(c)) * perTick).toLong

      // the spans: which recording thread each profiler thread is, per compilation, and the completions
      val records = tls.map(_.spanRecords)
      val steps = records.iterator.flatMap(_.iterator.map(_._2)).toSet
      val labels = records.iterator.flatMap(_.iterator.map(_._3)).toSet
      val threadOf = tls.map(_ => new java.util.HashMap[Integer, Integer]())
      val byThread = new java.util.HashMap[Integer, mutable.ArrayBuffer[SpanEvent]]()
      var span = jfr.readEvent(classOf[SpanEvent])
      while (span ne null) {
        if ((span.tag ne null) && span.tag.startsWith(ParallelTyperTimeline.ThreadTagPrefix))
          { val c = compilationAt(span.time); if (c >= 0) threadOf(c).put(span.tid, span.tag.substring(ParallelTyperTimeline.ThreadTagPrefix.length).toInt) }
        else if (span.tag ne null) byThread.computeIfAbsent(span.tid, _ => mutable.ArrayBuffer.empty[SpanEvent]) += span
        span = jfr.readEvent(classOf[SpanEvent])
      }
      jfr.rewind()
      val symbols = mutable.ArrayBuffer.empty[String]
      val symbolIds = new java.util.HashMap[String, Integer]()
      def symbolId(s: String): Int = {
        val i = symbolIds.get(s)
        if (i ne null) i.intValue else { symbols += s; symbolIds.put(s, symbols.length - 1); symbols.length - 1 }
      }
      // per thread, the completions by start, with the completion enclosing each: a span is a completion if it is
      // not a step or a label, or if it is inside a label (a symbol can be called `typer`)
      final class Completions(all: mutable.ArrayBuffer[SpanEvent]) {
        private val sorted = all.sortWith((a, b) => if (a.time != b.time) a.time < b.time else a.duration > b.duration).toArray
        private val parents = new Array[Int](sorted.length)
        private val isCompletion = new Array[Boolean](sorted.length)
        private val starts = sorted.map(_.time)
        locally {
          var stack = List.empty[Int]
          for (i <- sorted.indices) {
            val s = sorted(i)
            while (stack.nonEmpty && sorted(stack.head).time + sorted(stack.head).duration < s.time + s.duration) stack = stack.tail
            parents(i) = stack.headOption.getOrElse(-1)
            val insideLabel = stack.exists(j => labels(sorted(j).tag))
            isCompletion(i) = !labels(s.tag) && (insideLabel || !steps(s.tag))
            stack = i :: stack
          }
        }
        /** The innermost completion at this time, or -1. */
        def at(t: Long): Int = {
          var i = java.util.Arrays.binarySearch(starts, t)
          if (i < 0) i = -i - 2
          else while (i + 1 < starts.length && starts(i + 1) == t) i += 1
          while (i >= 0 && sorted(i).time + sorted(i).duration <= t) i = parents(i)
          while (i >= 0 && !isCompletion(i)) i = parents(i)
          if (i < 0) -1 else symbolId(sorted(i).tag)
        }
      }
      val completions = new java.util.HashMap[Integer, Completions]()
      byThread.forEach((t, ss) => completions.put(t, new Completions(ss)))

      val others = mutable.ArrayBuffer.empty[String]
      val otherIds = new java.util.HashMap[String, Integer]()
      // threads the recording does not know, by name without its number: -1 - their index in `others`
      def threadId(c: Int, tid: Int): Int = {
        val t = threadOf(c).get(tid)
        if (t ne null) t.intValue
        else {
          // `[Java: C2 CompilerThread0 tid=27907]`, `[tid=43583]`
          val n = String.valueOf(names.getThreadName(tid)).replaceAll("^\\[(Java: )?|\\s*tid=\\d+\\]$|[-# ]*\\d+$", "").replaceAll("[-# ]*\\d*$", "") match {
            case "" => "native"
            case x  => x
          }
          val i = otherIds.get(n)
          if (i ne null) -1 - i.intValue else { others += n; otherIds.put(n, others.length - 1); -others.length }
        }
      }

      val trie = new Trie
      val isJvm = mutable.ArrayBuffer.empty[Boolean]
      val stackNodes = new java.util.HashMap[Integer, Integer]()
      def stackNode(id: Int): Int = {
        val known = stackNodes.get(id)
        if (known ne null) return known.intValue
        val st = jfr.stackTraces.get(id)
        var node = -1
        if (st ne null) {
          // a recursive stack keeps a frame's outermost occurrence, and the real leaf (so that self time and each
          // frame's total stay right, and the depth is at most the number of distinct frames)
          val seen = new java.util.HashSet[Integer]()
          var i = st.methods.length - 1 // the stack is leaf first
          while (i >= 0) {
            val name = short(names.getMethodName(st.methods(i), st.types(i)))
            if (i == 0 || !isNoise(name)) {
              val f = trie.frameId(name)
              if (f == isJvm.length) isJvm += jvmFrame.matcher(name).find() || !name.contains('.')
              if (seen.add(f) || i == 0) node = trie.child(node, f)
            }
            i -= 1
          }
        }
        stackNodes.put(id, node)
        node
      }

      // a thread outside the recording (the JIT's, say) is one frame: jfrconv's flame graph has its stacks
      def otherNode(th: Int): Int = trie.child(-1, { val f = trie.frameId("[" + others(-1 - th) + "]"); if (f == isJvm.length) isJvm += true; f })

      // the projection onto the compilation shown: its span of each step and unit (the outer one if a unit has two),
      // and a compilation's own spans per thread
      type Rec = (Int, String, String, Long, Long)
      val target = new java.util.HashMap[(String, String), Rec]()
      for (r <- shown.spanRecords) {
        val k = (r._2, r._3); val o = target.get(k)
        if ((o eq null) || r._5 - r._4 > o._5 - o._4) target.put(k, r)
      }
      val spansOf: Array[Map[Int, Array[Rec]]] = records.map(_.groupBy(_._1).map { case (t, rs) => t -> rs.sortBy(_._4) })
      def spanAround(c: Int, tid: Int, t: Long): Rec = {
        val rs = spansOf(c).getOrElse(tid, null)
        if (rs eq null) return null
        var lo = 0; var hi = rs.length - 1; var i = -1
        while (lo <= hi) { val m = (lo + hi) >>> 1; if (rs(m)._4 <= t) { i = m; lo = m + 1 } else hi = m - 1 }
        var k = 0
        while (i >= 0 && k < 8) { if (rs(i)._5 > t) return rs(i); i -= 1; k += 1 }
        null
      }
      val shownThreads = shown.threadNames.toIndexedSeq
      val threadNames = tls.map(_.threadNames.toIndexedSeq)
      val wallOf = tls.map(_.wallMicros)
      /** The thread and time of a sample in the compilation shown; a time below 0 is a sample in no span there. */
      def project(c: Int, th: Int, t: Long): (Int, Long) = {
        val fraction = shown.wallMicros.toDouble / wallOf(c)
        def outside = {
          val onto = if (th < 0) th else math.max(0, shownThreads.indexOf(threadNames(c)(th)))
          (onto, -(1 + (t * fraction).toLong))
        }
        if (th < 0) return outside
        val own = spanAround(c, th, t)
        if (own eq null) return outside
        val to = target.get((own._2, own._3))
        if (to eq null) return outside
        val at = to._4 + ((t - own._4).toDouble / math.max(1L, own._5 - own._4) * (to._5 - to._4)).toLong
        (to._1, math.min(at, to._5 - 1))
      }

      // samples: thread, time (below 0: -1 - time, in no span), node, completion (-1: none), count
      val samples = new StringBuilder
      val states = jfr.enums.get("jdk.types.ThreadState")
      var count = 0
      def add(v: Long): Unit = { if (samples.nonEmpty) samples.append(','); samples.append(v) }
      var e = jfr.readEvent(classOf[ExecutionSample])
      while (e ne null) {
        val cIndex = compilationAt(e.time)
        if (cIndex >= 0) {
          val t = us(cIndex, e.time)
          val th0 = threadId(cIndex, e.tid)
          // wall: only the threads of the recording; the JVM's own sit idle in their loops
          if (!config.endsWith("-wall") || th0 >= 0) {
            val (th, at) = project(cIndex, th0, t)
            val c = completions.get(e.tid)
            add(th); add(at); add(if (th >= 0) stackNode(e.stackTraceId) else otherNode(th)); add(if (c eq null) -1 else c.at(e.time)); add(e.samples)
            count += e.samples
          }
        }
        e = jfr.readEvent(classOf[ExecutionSample])
      }
      def strs(xs: Iterable[String]) = xs.iterator.map(quote).mkString("[", ",", "]")
      val parent = trie.parent.result(); val frame = trie.frame.result()
      println(s"profile $config: $count samples, ${trie.nodes} nodes, ${trie.frames.length} frames, ${symbols.length} completions")
      val event = if (config.endsWith("-wall")) "wall" else "cpu"
      // a node's parent as its distance back: mostly the node before
      val parentBack = parent.indices.map(i => if (parent(i) < 0) -1 else i - parent(i) - 1)
      s"""{"dir":${quote(s"profiles/$corpus")},"jfr":${quote(s"run-$config.jfr")},"event":${quote(event)},"interval":${quote(interval)},"repeats":${tls.length},
         |"frames":${strs(trie.frames)},
         |"jvmFrames":[${trie.frames.indices.filter(isJvm).mkString(",")}],
         |"parent":[${parentBack.mkString(",")}],
         |"frame":[${frame.mkString(",")}],
         |"symbols":${strs(symbols)},
         |"others":${strs(others)},
         |"samples":[$samples]}""".stripMargin
    } finally jfr.close()
  }

  private def quote(s: String): String = {
    val b = new java.lang.StringBuilder(s.length + 2).append('"')
    s.foreach {
      case '"' => b.append("\\\""); case '\\' => b.append("\\\\")
      case c if c < ' ' => b.append(' ')
      case c => b.append(c)
    }
    b.append('"').toString
  }
}
