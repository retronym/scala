// A clean build of a multi-module project in one JVM with nothing on disk and no incremental compiler: a Global per
// module, on a budget of workers, traditionally or pipelined. Reads the module list from bloop_modules.py.
//
//   XrayBuild <modules file> <warmup token> <warmup> <rounds> <token>...
//
// A token is <strategy>:<policy>. The strategy is `trad` (a module starts when its dependencies have class files) or
// `pipe` (when they have pickles; a module that provides macros, and anything that a dependency's macro implementation
// needs, still has to finish first). The policy is `<M>x<T>` (at most M modules at once, each on T typer threads, T=1
// the serial compiler), `adapt<C>` (C workers; a module on the critical path, or with nothing else ready, takes what
// is free, up to 8, and the others take one) or `bal<C>` (C workers; a module takes the fewest that would finish it by the
// time the remaining work, shared over C, would be done, and what is free if nothing else is ready). A compile keeps its
// workers until it ends. Ready modules go in order of the work waiting on them, from the
// module's size or, once a round has been timed, its measured time.
//
// Pickles of everything, Java sources included, are published as .sig files at the end of the pickler phase, in both
// strategies: javac is not run, so a Java class exists downstream only as a signature. Each round is one `R {json}` line.
package scala.tools.nsc.xray

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{AbstractFileClassLoader, SourceFile}
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import scala.tools.nsc.reporters.StoreReporter

final case class Mod(name: String, deps: List[String], upstream: List[String], jars: List[String], sources: List[String], options: List[String], lines: Long, providesMacros: Boolean, resources: List[String])

final class Outputs(name: String) {
  val classes = new VirtualDirectory(s"classes-$name", None)
  val sigs = new VirtualDirectory(s"sigs-$name", None)
}

object XrayBuild {
  def parse(file: String): List[Mod] = {
    val out = mutable.ListBuffer.empty[Mod]
    var name = ""; var lines = 0L; var macros = false
    val deps, up, jars, srcs, opts, res = mutable.ListBuffer.empty[String]
    def flush(): Unit = if (name.nonEmpty) {
      out += Mod(name, deps.toList, up.toList, jars.toList, srcs.toList, opts.toList, lines, macros, res.toList)
      deps.clear(); up.clear(); jars.clear(); srcs.clear(); opts.clear(); res.clear()
    }
    Files.readAllLines(Paths.get(file)).asScala.foreach { l =>
      val v = l.drop(2)
      l.take(1) match {
        case "M" => flush(); name = v; lines = 0; macros = false
        case "L" => lines = v.toLong
        case "P" => macros = true
        case "D" => deps += v
        case "U" => up += v
        case "J" => jars += v
        case "S" => srcs += v
        case "O" => opts += v
        case "R" => res += v
        case _   =>
      }
    }
    flush()
    out.toList
  }

  private val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private val os = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
  private val gcCpu: () => Long = {
    val mem = ManagementFactory.getMemoryMXBean
    try { val m = classOf[java.lang.management.MemoryMXBean].getMethod("getTotalGcCpuTime"); () => m.invoke(mem).asInstanceOf[java.lang.Long].longValue }
    catch { case _: NoSuchMethodException => () => -1L }
  }

  // what a compile of ScalaTest gets from its threads, and from the cores of a whole build
  private val balanceSpeedup = List(1 -> 1.0, 2 -> 1.75, 3 -> 2.4, 4 -> 3.0, 6 -> 3.6, 8 -> 4.0)
  private val balanceEfficiency = 0.65
  private val sayAll = System.getProperty("xray.build.errors") != null
  private lazy val ourLibrary = sys.props("java.class.path").split(java.io.File.pathSeparator).find(p => p.endsWith("/library") || p.contains("scala-library")).get

  final class ModResult(var start: Long = 0, var pickled: Long = 0, var end: Long = 0, var workers: Int = 0, var cpuMs: Double = 0, var allocMB: Double = 0, var errors: Int = 0)

  /** One module's compile. `visible(m)` is what of m is on the classpath: its signatures, its class files or both. */
  private def compile(m: Mod, byName: Map[String, Mod], order: Map[String, Int], outs: Map[String, Outputs], pipelined: Boolean, workers: Int, res: ModResult, t0: Long, onPickles: () => Unit, sayErrors: AtomicBoolean): Unit = {
    if (m.sources.isEmpty) { res.pickled = System.nanoTime - t0; onPickles(); return }
    val s = new Settings(msg => sys.error(msg))
    val parallel = if (workers > 1) List("-Yparallel-typer", "-Yparallel-typer-threads", workers.toString) else Nil
    val (ok, rest) = s.processArguments(m.options ++ parallel, processAll = true)
    assert(ok, rest)
    val out = outs(m.name)
    s.outputDirs.setSingleOutput(out.classes)
    s.YpickleJava.value = true
    // deepest first: a class loader sees its parent, and a macro implementation sees what its module depends on
    val providers = m.upstream.flatMap(byName.get).filter(_.providesMacros).sortBy(p => order(p.name))
    // A dependency's class files are there only if the module waited for them; it always has its signatures, which are
    // all there is of its Java classes.
    val upstream = m.upstream.filter(byName.contains).flatMap { u =>
      val o = outs(u)
      val classes = if (!pipelined || byName(u).providesMacros) List(o.classes) else Nil
      classes :+ o.sigs
    }
    // a project of Java alone has no library on its classpath, and a compiler needs one
    val library = if (m.jars.exists(_.contains("scala-library"))) Nil else List(InMemory.classRoot(ourLibrary))
    val cp = AggregateClassPath((InMemory.jdk :: upstream ::: m.jars.map(InMemory.classRoot) ::: library).map(VirtualDirectoryClassPath(_)))
    val reporter = new StoreReporter(s)
    val g = new Global(s, reporter) {
      override lazy val platform: ThisPlatform = new GlobalPlatform {
        currentClassPath = Some(cp)
        override def needCompile(bin: AbstractFile, src: AbstractFile): Boolean = false
      }
      // macro implementations from the providers' output in memory, their resource bundles from disk, and the libraries
      // from the JVM's own classpath
      override def findMacroClassLoader(): ClassLoader = {
        val resources = providers.flatMap(_.resources).distinct.map(r => Paths.get(r).toUri.toURL).toArray
        val base: ClassLoader = new java.net.URLClassLoader(resources, classOf[Global].getClassLoader)
        providers.foldLeft(base) { (parent, p) => new AbstractFileClassLoader(outs(p.name).classes, parent) }
      }
    }
    val published = new AtomicBoolean(false)
    def publish(run: g.Run): Unit = if (published.compareAndSet(false, true)) {
      g.enteringPhase(run.picklerPhase) {
        val written = mutable.Set.empty[String]
        run.symData.foreach { case (sym, pickle) =>
          val binary = sym.javaBinaryNameString
          val path = (if (sym.isModule) binary.stripSuffix("$") else binary) + ".sig"
          if (written.add(path)) add(out.sigs, path, pickle.bytes.take(pickle.writeIndex))
        }
      }
      res.pickled = System.nanoTime - t0
      onPickles()
    }
    val run: g.Run = new g.Run {
      override def advancePhase(): Unit = {
        if (g.phase == this.picklerPhase) publish(this)
        super.advancePhase()
      }
    }
    run.compileSources(m.sources.map(InMemory.source))
    publish(run) // a compile that stopped early, on errors, must not hold up the build
    val errors = reporter.infos.filter(_.severity == reporter.ERROR)
    res.errors = errors.size
    if (errors.nonEmpty && (sayAll || sayErrors.compareAndSet(false, true)))
      errors.take(3).foreach(e => Console.err.println(s"error: ${m.name}: ${e.pos}: ${e.msg.take(300)}"))
  }

  private def add(root: AbstractFile, rel: String, data: Array[Byte]): Unit = {
    val parts = rel.split('/').toList
    val dir = parts.init.foldLeft(root)(_.subdirectoryNamed(_))
    val o = dir.fileNamed(parts.last).output
    try o.write(data) finally o.close()
  }

  /** The work waiting on each module, longest chain to the end of the build. */
  private def priorities(mods: List[Mod], work: String => Double, pre: String => Double): Map[String, Double] = {
    val users = mods.flatMap(m => m.deps.map(_ -> m.name)).groupMap(_._1)(_._2)
    val memo = mutable.Map.empty[String, Double]
    def rem(n: String): Double = memo.getOrElseUpdate(n, (work(n) :: users.getOrElse(n, Nil).map(u => pre(n) + rem(u))).max)
    mods.map(m => m.name -> rem(m.name)).toMap
  }

  def build(mods: List[Mod], token: String, round: Int, timed: ConcurrentHashMap[String, (Double, Double)]): String = {
    val Array(strategy, policy) = token.split(":", 2)
    val pipelined = strategy == "pipe"
    val adaptive = policy.startsWith("adapt")
    val balanced = policy.startsWith("bal")
    val (slots, each) =
      if (adaptive) (policy.drop(5).toInt, 0)
      else if (balanced) (policy.drop(3).toInt, 0)
      else { val Array(a, b) = policy.split("x"); (a.toInt, b.toInt) }
    val byName = mods.map(m => m.name -> m).toMap
    val order = mods.iterator.map(_.name).zipWithIndex.toMap // the module file is in dependency order
    val outs = mods.map(m => m.name -> new Outputs(m.name)).toMap
    val res = mods.map(m => m.name -> new ModResult).toMap
    def work(n: String) = Option(timed.get(n)).map(_._1).getOrElse(byName(n).lines.toDouble)
    def pre(n: String) = Option(timed.get(n)).map(_._2).getOrElse(0.4 * byName(n).lines)
    val pri = priorities(mods, work, pre)
    // what a module waits for: pickles of its dependencies when pipelined, class files of whatever provides macros
    val needs = mutable.Map.empty[String, (Set[String], Set[String])] // pickles of, classes of
    for (m <- mods) {
      val onCp = m.upstream.filter(byName.contains)
      val pickles = if (pipelined) m.deps.filter(byName.contains).toSet else Set.empty[String]
      val classes = if (pipelined) onCp.filter(byName(_).providesMacros).toSet else m.deps.filter(byName.contains).toSet
      needs(m.name) = (pickles -- classes, classes)
    }
    val lock = new Object
    val picklesDone, classesDone, started = mutable.Set.empty[String]
    var used = 0; var running = 0
    val finished = new CountDownLatch(mods.size)
    val say = new AtomicBoolean(false)
    def ready(n: String) = !started(n) && needs(n)._1.subsetOf(picklesDone) && needs(n)._2.subsetOf(classesDone)
    val t0 = System.nanoTime
    val wall0 = System.currentTimeMillis; val proc0 = os.getProcessCpuTime; val gc0 = gcCpu()

    def next(): Option[(Mod, Int)] = lock.synchronized {
      var chosen: Option[(Mod, Int)] = None
      while (chosen.isEmpty && started.size < mods.size) {
        val rs = mods.iterator.map(_.name).filter(ready).toList.sortBy(n => -pri(n))
        val free = if (adaptive || balanced) slots - used else slots - running
        if (rs.nonEmpty && free > 0) {
          val n = rs.head
          val w =
            if (balanced) {
              val left = mods.iterator.map(_.name).filterNot(classesDone).map(work).sum
              val target = left / (slots * balanceEfficiency)
              if (rs.size == 1) math.min(8, free)
              else math.min(free, balanceSpeedup.collectFirst { case (t, sp) if work(n) / sp <= target => t }.getOrElse(8))
            }
            else if (!adaptive) each
            else {
              val top = mods.iterator.map(_.name).filterNot(classesDone).map(pri).max
              // the critical path takes what is free, and so does a module with nothing else to run beside it
              if (pri(n) >= 0.9 * top || rs.size == 1) math.min(8, free) else 1
            }
          started += n; running += 1; used += w
          chosen = Some((byName(n), w))
        } else lock.wait()
      }
      chosen
    }

    val runners = (1 to slots).map { i =>
      new Thread(null, () => {
        var more = true
        while (more) next() match {
          case None => more = false
          case Some((m, w)) =>
            val r = res(m.name)
            r.workers = w; r.start = System.nanoTime - t0
            val id = Thread.currentThread.getId
            val c0 = threads.getThreadCpuTime(id); val a0 = threads.getThreadAllocatedBytes(id)
            try compile(m, byName, order, outs, pipelined, if (w <= 1) 1 else w, r, t0, () => lock.synchronized { picklesDone += m.name; lock.notifyAll() }, say)
            catch { case t: Throwable => r.errors += 1; if (say.compareAndSet(false, true)) t.printStackTrace() }
            r.end = System.nanoTime - t0
            r.cpuMs = (threads.getThreadCpuTime(id) - c0) / 1e6; r.allocMB = (threads.getThreadAllocatedBytes(id) - a0) / 1e6
            lock.synchronized { picklesDone += m.name; classesDone += m.name; running -= 1; used -= w; lock.notifyAll() }
            finished.countDown()
        }
      }, s"build-$i", 1L << 28)
    }
    runners.foreach(_.start()); runners.foreach(_.join())
    val nanos = System.nanoTime - t0; val wall1 = System.currentTimeMillis
    // a serial compile, alone, times the module for the priorities and the shares of later rounds
    if (policy == "1x1") for ((n, r) <- res) timed.put(n, ((r.end - r.start) / 1e6, (r.pickled - r.start) / 1e6))
    val ms = res.toList.sortBy(_._2.start).map { case (n, r) =>
      f"""{"n":"$n","t":${r.workers},"s":${r.start / 1e6}%.1f,"p":${r.pickled / 1e6}%.1f,"e":${r.end / 1e6}%.1f,"cpu":${r.cpuMs}%.1f,"alloc":${r.allocMB}%.0f,"err":${r.errors}}"""
    }
    f"""R {"token":"$token","round":$round,"batch":${nanos / 1e6}%.1f,"errors":${res.values.map(_.errors).sum},"procCpu":${(os.getProcessCpuTime - proc0) / 1e6}%.1f,"gcCpu":${(gcCpu() - gc0) / 1e6}%.1f,"t0":$wall0,"t1":$wall1,"nanoMs":${nanos / 1000000},"load":${os.getSystemLoadAverage}%.2f,"mods":[${ms.mkString(",")}]}"""
  }

  def main(args: Array[String]): Unit = {
    val mods = parse(args(0))
    val warmToken = args(1); val warmup = args(2).toInt; val rounds = args(3).toInt; val tokens = args.drop(4).toList
    val timed = new ConcurrentHashMap[String, (Double, Double)]
    for (i <- 1 to warmup) {
      val t0 = System.nanoTime
      val line = build(mods, warmToken, -i, timed)
      val errs = "\"errors\":(\\d+)".r.findFirstMatchIn(line).map(_.group(1)).getOrElse("?")
      println(f"warm-up $i%2d $warmToken ${(System.nanoTime - t0) / 1e9}%.1f s, errors $errs")
    }
    println("ready")
    // in a different order each round, so that no token always follows the same one; the serial baseline first, to time
    // the modules
    for (r <- 1 to rounds) {
      val shuffled = new scala.util.Random(r).shuffle(tokens)
      val ordered = if (r == 1) { val (a, b) = shuffled.partition(_.endsWith(":1x1")); a ++ b } else shuffled
      for (t <- ordered) { System.gc(); println(build(mods, t, r, timed)) }
    }
  }
}
