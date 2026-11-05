/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util

import java.lang.management.ManagementFactory
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks.ReentrantLock

import scala.collection.mutable

/** The x-ray recording of a compiler run (`tools/parallel-typer/xray`): what each thread did when, and every wait for
 *  the completion lock with the hold it waited on.
 *
 *  - A span per unit per step (each phase's `applyPhase`, the typer's signatures pre-pass, annotation sub-pass and
 *    lazy-val forcing) with its wall time, the thread's CPU time, the time the thread was blocked on a monitor and
 *    parked (`ThreadMXBean`, milliseconds), and its time waiting for and holding the completion lock.
 *  - Every wait for the completion lock (request to acquisition), with its call site and, where the caller says,
 *    what was being completed; every outermost hold that someone waited on or that took longer than a threshold
 *    (`scalac.parallelTyper.xray.holdMicros`, default 50); per call site, the count and totals of all of them.
 *  - The phases on the main thread, GC pauses, and the moment each typer worker ran out of units.
 *
 *  Off unless `ParallelTyperTimeline.current` is set: the hooks test it for null. The lock's events are recorded
 *  while the lock is held, so their tables need no synchronisation of their own.
 */
final class ParallelTyperTimeline(val holdThresholdNanos: Long) {
  import ParallelTyperTimeline._

  val startNanos: Long = System.nanoTime()
  val startEpochMillis: Long = System.currentTimeMillis()
  @volatile private[this] var endNanos = 0L
  val meta = new java.util.LinkedHashMap[String, String]()
  contentionMonitoring(true)

  // threads, by first appearance
  private[this] val threadNames = mutable.ArrayBuffer.empty[String]
  private[this] val threadIndex = new ThreadLocal[Integer]
  private def tid(): Int = {
    val i = threadIndex.get
    if (i ne null) i.intValue
    else threadNames.synchronized {
      threadNames += Thread.currentThread.getName
      threadIndex.set(threadNames.length - 1)
      threadNames.length - 1
    }
  }

  // ---- spans

  private final class Span(val tid: Int, val step: String, val label: String, val t0: Long, val t1: Long, val cpu: Long,
                           val blockedMs: Long, val waitedMs: Long, val lockWait: Long, val lockHold: Long)
  private[this] val spans = new ConcurrentLinkedQueue[Span]()
  private[this] val phases = new ConcurrentLinkedQueue[Array[Any]]()
  private[this] val idleFrom = new ConcurrentLinkedQueue[Array[Long]]()

  /** The start of a span on the current thread, for `end`. */
  def begin(): Start = {
    val ti = threadInfo()
    val times = PerThreadTimes.current
    new Start(System.nanoTime(), cpuNow(), if (ti eq null) -1 else ti.getBlockedTime, if (ti eq null) -1 else ti.getWaitedTime,
      times.waitNanos, times.holdNanos)
  }
  def end(s: Start, step: String, label: String): Unit = {
    val t1 = System.nanoTime()
    val cpu = cpuNow() - s.cpu
    val ti = threadInfo()
    val times = PerThreadTimes.current
    spans.add(new Span(tid(), step, label, s.t0, t1, cpu,
      if ((ti eq null) || s.blocked < 0) -1 else ti.getBlockedTime - s.blocked,
      if ((ti eq null) || s.waited < 0) -1 else ti.getWaitedTime - s.waited,
      times.waitNanos - s.lockWait, times.holdNanos - s.lockHold))
  }
  def phase(name: String, t0: Long, t1: Long): Unit = phases.add(Array[Any](name, t0, t1))
  /** The current thread (a typer worker) found no more units. */
  def idle(): Unit = idleFrom.add(Array(tid().toLong, System.nanoTime()))

  // ---- the completion lock; every field below is written while holding it

  private[this] val sites = new java.util.HashMap[String, Integer]()
  private[this] val siteNames = mutable.ArrayBuffer.empty[String]
  // per site: count, hold total, hold max, waits, wait total, wait max, waiters (a bit per thread index)
  private[this] var siteStats = new Array[Long](64 * SiteStride)
  private[this] val whats = new java.util.IdentityHashMap[AnyRef, Integer]()
  private[this] val whatList = mutable.ArrayBuffer.empty[AnyRef]
  private[this] val holds = new LongBuf // tid, site, what, t0, t1, contended
  private[this] val waits = new LongBuf // tid, site, what, t0, t1
  private[this] var holdStart, holdTid, holdSite, holdWhat = 0L

  private def siteId(label: String): Int = {
    val i = sites.get(label)
    if (i ne null) i.intValue
    else {
      val n = siteNames.length
      siteNames += label
      sites.put(label, n)
      if ((n + 1) * SiteStride > siteStats.length) siteStats = java.util.Arrays.copyOf(siteStats, siteStats.length * 2)
      n
    }
  }
  private def whatId(what: AnyRef): Long =
    if (what eq null) -1
    else {
      val i = whats.get(what)
      if (i ne null) i.longValue
      else { whatList += what; whats.put(what, whatList.length - 1); whatList.length - 1L }
    }

  /** `CompletionLock.acquire` while recording: as there, and the wait with its site and what it was for. */
  def acquire(cl: CompletionLock, lock: ReentrantLock, what: AnyRef): Unit = {
    // the caller's site, before the lock is taken: the stack walk is neither wait nor hold
    val site = if (lock.isHeldByCurrentThread) null else callSite()
    val me = tid()
    if (!lock.tryLock()) {
      val t0 = System.nanoTime()
      lock.lock()
      val t1 = System.nanoTime()
      cl.waitNanos.add(t1 - t0); cl.waits.increment(); PerThreadTimes.addWait(t1 - t0)
      val s = siteId(site)
      val w = whatId(what)
      waits.add5(me, s, w, t0, t1)
      val o = s * SiteStride
      val st = siteStats
      st(o + 3) += 1; st(o + 4) += t1 - t0; if (t1 - t0 > st(o + 5)) st(o + 5) = t1 - t0; st(o + 6) |= 1L << (me & 63)
    }
    if (lock.getHoldCount == 1) {
      cl.acquisitions.increment()
      holdTid = me; holdSite = siteId(site); holdWhat = if (what eq null) -2 else whatId(what)
      holdStart = System.nanoTime()
    }
  }

  def release(cl: CompletionLock, lock: ReentrantLock): Unit = {
    if (lock.getHoldCount == 1) {
      val t1 = System.nanoTime()
      val held = t1 - holdStart
      cl.holdNanos.add(held); PerThreadTimes.addHold(held)
      val o = holdSite.toInt * SiteStride
      val st = siteStats
      st(o) += 1; st(o + 1) += held; if (held > st(o + 2)) st(o + 2) = held
      // a thread queued now arrived during this hold (it cannot leave the queue before this release)
      val contended = lock.hasQueuedThreads
      if (contended || held >= holdThresholdNanos)
        holds.add6(holdTid, holdSite, holdWhat, holdStart, t1, if (contended) 1 else 0)
    }
    lock.unlock()
  }

  // ---- GC pauses, from the collectors' notifications

  private[this] val gcs = new ConcurrentLinkedQueue[Array[Any]]()
  private[this] val gcListeners: List[(javax.management.NotificationEmitter, javax.management.NotificationListener)] = {
    import javax.management._
    import javax.management.openmbean.CompositeData
    val jvmStartNanos = System.nanoTime() - ManagementFactory.getRuntimeMXBean.getUptime * 1000000L
    val beans = ManagementFactory.getGarbageCollectorMXBeans
    (0 until beans.size).toList.map(beans.get).collect { case e: NotificationEmitter =>
      val l: NotificationListener = (n: Notification, _: AnyRef) => n.getUserData match {
        case cd: CompositeData if n.getType == "com.sun.management.gc.notification" =>
          val name = String.valueOf(cd.get("gcName")); val cause = String.valueOf(cd.get("gcCause"))
          // concurrent cycles are not pauses
          if (!name.contains("Concurrent") && !cause.contains("No GC")) {
            val info = cd.get("gcInfo").asInstanceOf[CompositeData]
            val s = info.get("startTime").asInstanceOf[java.lang.Long].longValue
            val e = info.get("endTime").asInstanceOf[java.lang.Long].longValue
            gcs.add(Array[Any](s"$name: $cause (${e - s} ms)", jvmStartNanos + s * 1000000L, jvmStartNanos + e * 1000000L))
          }
        case _ =>
      }
      e.addNotificationListener(l, null, null)
      (e, l)
    }
  }

  /** The run is over: stop listening for GC pauses; `render` names what the lock was taken for (a symbol). */
  def finish(render: AnyRef => String): Unit = {
    endNanos = System.nanoTime()
    contentionMonitoring(false)
    gcListeners.foreach { case (e, l) => try e.removeNotificationListener(l) catch { case _: Exception => () } }
    this.render = render
  }
  @volatile private[this] var render: AnyRef => String = String.valueOf(_)

  // ---- JSON

  /** The recording as JSON, times in microseconds from its start; see tools/parallel-typer/xray/README.md. */
  def toJson: String = {
    val end = if (endNanos == 0) System.nanoTime() else endNanos
    val strs = new java.util.HashMap[String, Integer]()
    val strList = mutable.ArrayBuffer.empty[String]
    def s(x: String): Int = { val i = strs.get(x); if (i ne null) i.intValue else { strList += x; strs.put(x, strList.length - 1); strList.length - 1 } }
    def us(ns: Long): Long = (ns - startNanos) / 1000
    def dur(ns: Long): Long = ns / 1000
    val sb = new java.lang.StringBuilder(1 << 20)
    def arr(name: String)(body: (Any => Unit) => Unit): Unit = {
      sb.append(",\n\"").append(name).append("\":[")
      var first = true
      body { v => if (first) first = false else sb.append(','); sb.append(v) }
      sb.append(']')
    }
    sb.append("{\"version\":1,\"startEpochMillis\":").append(startEpochMillis).append(",\"wallUs\":").append(dur(end - startNanos))
      .append(",\"holdThresholdUs\":").append(dur(holdThresholdNanos))
    sb.append(",\n\"meta\":{")
    val m = new java.util.LinkedHashMap[String, String](environment); m.putAll(meta)
    var first = true
    m.forEach { (k, v) => if (first) first = false else sb.append(','); sb.append(quote(k)).append(':').append(quote(v)) }
    sb.append('}')
    // spans: tid, step, label, t0, t1, cpu, blocked ms, waited ms, lock wait, lock hold
    val ss = spans.toArray(new Array[Span](0)).sortBy(_.t0)
    arr("spans")(a => ss.foreach { x =>
      a(x.tid); a(s(x.step)); a(s(x.label)); a(us(x.t0)); a(us(x.t1)); a(dur(x.cpu)); a(x.blockedMs); a(x.waitedMs); a(dur(x.lockWait)); a(dur(x.lockHold))
    })
    arr("phases")(a => phases.forEach { p => a(s(p(0).asInstanceOf[String])); a(us(p(1).asInstanceOf[Long])); a(us(p(2).asInstanceOf[Long])) })
    arr("idle")(a => idleFrom.forEach { p => a(p(0)); a(us(p(1))) })
    arr("gc")(a => gcs.forEach { g => a(s(g(0).asInstanceOf[String])); a(us(g(1).asInstanceOf[Long])); a(us(g(2).asInstanceOf[Long])) })
    // the lock's tables are complete: the run is over, nobody holds it
    val siteStr = siteNames.map(n => s(if (n eq null) "(reentrant)" else n))
    val whatStr = whatList.map(w => s(try render(w) catch { case _: Throwable => String.valueOf(w) }))
    def what(i: Long): Int = if (i < 0) -1 else whatStr(i.toInt)
    arr("holds")(a => holds.foreach(6) { (b, i) => a(b(i)); a(siteStr(b(i + 1).toInt)); a(what(b(i + 2))); a(us(b(i + 3))); a(us(b(i + 4))); a(b(i + 5)) })
    arr("waits")(a => waits.foreach(5) { (b, i) => a(b(i)); a(siteStr(b(i + 1).toInt)); a(what(b(i + 2))); a(us(b(i + 3))); a(us(b(i + 4))) })
    // sites: name, acquisitions, hold total, hold max, waits, wait total, wait max, waiters (bits by thread index)
    arr("sites")(a => siteNames.indices.foreach { i =>
      val o = i * SiteStride; val st = siteStats
      a(siteStr(i)); a(st(o)); a(dur(st(o + 1))); a(dur(st(o + 2))); a(st(o + 3)); a(dur(st(o + 4))); a(dur(st(o + 5))); a(st(o + 6))
    })
    arr("threads")(a => threadNames.synchronized(threadNames.foreach(n => a(quote(n)))))
    arr("strings")(a => strList.foreach(x => a(quote(x))))
    sb.append("}\n").toString
  }
}

object ParallelTyperTimeline {
  /** The recording in progress, or null: the hooks' one test. Set before a run starts, cleared after it ends. */
  var current: ParallelTyperTimeline = null
  private var fromProperty: String = null

  final class Start(val t0: Long, val cpu: Long, val blocked: Long, val waited: Long, val lockWait: Long, val lockHold: Long)

  private final val SiteStride = 7

  def threshold: Long = java.lang.Long.getLong("scalac.parallelTyper.xray.holdMicros", 50L) * 1000L

  /** A run starts: with `-Dscalac.parallelTyper.xray=<file>`, record it (unless a recording is already installed). */
  def runStarting(): ParallelTyperTimeline = {
    if (current eq null) {
      val file = System.getProperty("scalac.parallelTyper.xray")
      if (file != null) { current = new ParallelTyperTimeline(threshold); fromProperty = file }
    }
    current
  }
  /** The run is over: finish the recording, and write it if the property asked for it. */
  def runFinished(tl: ParallelTyperTimeline, render: AnyRef => String): Unit = if (tl ne null) {
    tl.finish(render)
    if ((fromProperty ne null) && (current eq tl)) {
      java.nio.file.Files.write(java.nio.file.Paths.get(fromProperty), tl.toJson.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      current = null; fromProperty = null
    }
  }

  /** `body` as a span of `step` on this thread, when recording. */
  @inline def span[T](step: String, label: => String)(body: => T): T = {
    val tl = current
    if (tl eq null) body
    else {
      val s = tl.begin()
      try body finally tl.end(s, step, label)
    }
  }

  // ---- per thread measurements

  private lazy val threadBean = ManagementFactory.getThreadMXBean
  // blocked and waited times per thread: off by default in the JVM, on while recording
  private def contentionMonitoring(on: Boolean): Unit =
    if (threadBean.isThreadContentionMonitoringSupported) threadBean.setThreadContentionMonitoringEnabled(on)
  private def cpuNow(): Long = threadBean.getCurrentThreadCpuTime
  private[this] val threadInfoOn = !"false".equals(System.getProperty("scalac.parallelTyper.xray.threadInfo"))
  private def threadInfo(): java.lang.management.ThreadInfo = if (threadInfoOn) threadBean.getThreadInfo(Thread.currentThread.getId) else null

  // ---- call sites: the first frame outside the lock's plumbing, as `Class.method` (StackWalker, read reflectively
  // so that this compiles for Java 8; a stack trace there)

  private[this] val plumbing = Set("acquire", "release", "apply", "underCompletionLock", "completeInfoUnderLock", "await",
    "awaitCompletionElsewhere", "callSite", "walk")
  private def isPlumbing(cls: String, method: String): Boolean =
    plumbing(method) || cls.startsWith("jdk.") || cls.endsWith("CompletionLock") || cls.endsWith("ParallelTyperTimeline") || cls.endsWith("ParallelTyperTimeline$") ||
      cls == "scala.reflect.internal.SymbolTable" || cls.startsWith("java.")
  private[util] def label(cls: String, method: String): String = {
    val simple = cls.substring(cls.lastIndexOf('.') + 1).split('$').filter(p => p.nonEmpty && !p.forall(_.isDigit) && p != "class")
    val owner = if (simple.isEmpty) cls else simple.last
    // a closure's body (`$anonfun$name$1`) and a local method (`name$1`) are in `name`; `name$` is a trait's static forwarder
    val parts = method.split('$').filter(p => p.nonEmpty && p != "anonfun" && p != "adapted" && !p.forall(_.isDigit))
    s"$owner.${parts.headOption.getOrElse(method)}"
  }
  // per class: whether it is plumbing, and the label of each of its methods (computed once: the walk is per
  // outermost acquisition, a hundred thousand a run)
  private final class ClassSites(cls: Class[_]) {
    val isPlumbing: Boolean = ParallelTyperTimeline.isPlumbing(cls.getName, "")
    val labels = new java.util.concurrent.ConcurrentHashMap[String, String]()
    def label(m: String): String = {
      val l = labels.get(m)
      if (l ne null) l
      else {
        // a closure's body in a plumbing method (`$anonfun$apply$1`) is plumbing
        val base = if (m.startsWith("$anonfun$")) m.stripPrefix("$anonfun$").takeWhile(_ != '$') else m
        val v = if (plumbing(m) || plumbing(base)) "" else ParallelTyperTimeline.label(cls.getName, m).intern()
        labels.putIfAbsent(m, v); v
      }
    }
  }
  private[this] val classSites = new ClassValue[ClassSites] { def computeValue(c: Class[_]) = new ClassSites(c) }
  private[this] final class SiteFinder extends java.util.function.Function[java.util.stream.Stream[AnyRef], String] {
    def apply(frames: java.util.stream.Stream[AnyRef]): String = {
      val it = frames.iterator
      while (it.hasNext) {
        val f = it.next()
        val cs = classSites.get(getDeclaringClass.invoke(f).asInstanceOf[Class[_]])
        if (!cs.isPlumbing) {
          val l = cs.label(getMethodName.invoke(f).asInstanceOf[String])
          if (!l.isEmpty) return l
        }
      }
      "(unknown)"
    }
  }
  private[this] val (walker, walk, getDeclaringClass, getMethodName) = try {
    val sw = Class.forName("java.lang.StackWalker")
    val sf = Class.forName("java.lang.StackWalker$StackFrame")
    val opt = Class.forName("java.lang.StackWalker$Option")
    val retain = opt.getField("RETAIN_CLASS_REFERENCE").get(null)
    val lookup = java.lang.invoke.MethodHandles.publicLookup
    (sw.getMethod("getInstance", opt).invoke(null, retain), sw.getMethod("walk", classOf[java.util.function.Function[_, _]]),
      lookup.unreflect(sf.getMethod("getDeclaringClass")), lookup.unreflect(sf.getMethod("getMethodName")))
  } catch { case _: Throwable => (null, null, null, null) }
  private[this] val finder = new SiteFinder
  private[this] val sitesOn = !"false".equals(System.getProperty("scalac.parallelTyper.xray.sites"))
  def callSite(): String =
    if (!sitesOn) "(sites off)"
    else if (walker ne null) walk.invoke(walker, finder).asInstanceOf[String]
    else new Throwable().getStackTrace.find(f => !isPlumbing(f.getClassName, f.getMethodName)).map(f => label(f.getClassName, f.getMethodName)).getOrElse("(unknown)")

  // ---- JSON helpers and the environment

  private def quote(s: String): String = {
    val b = new java.lang.StringBuilder(s.length + 2).append('"')
    s.foreach {
      case '"' => b.append("\\\""); case '\\' => b.append("\\\\")
      case c if c < ' ' => b.append(' ')
      case c => b.append(c)
    }
    b.append('"').toString
  }

  private lazy val environment: java.util.Map[String, String] = {
    val m = new java.util.LinkedHashMap[String, String]()
    def sysctl(k: String): String = try {
      val p = new ProcessBuilder("sysctl", "-n", k).redirectErrorStream(true).start()
      val out = scala.io.Source.fromInputStream(p.getInputStream, "UTF-8").mkString.trim
      if (p.waitFor() == 0) out else ""
    } catch { case _: Throwable => "" }
    val rt = ManagementFactory.getRuntimeMXBean
    m.put("jvm", s"${System.getProperty("java.vm.name")} ${System.getProperty("java.runtime.version")}")
    m.put("jvmArgs", String.join(" ", rt.getInputArguments))
    m.put("command", String.valueOf(System.getProperty("sun.java.command")))
    m.put("processors", Runtime.getRuntime.availableProcessors.toString)
    m.put("maxHeapMb", (Runtime.getRuntime.maxMemory >> 20).toString)
    val gcs = ManagementFactory.getGarbageCollectorMXBeans
    m.put("gc", (0 until gcs.size).map(i => gcs.get(i).getName).mkString(", "))
    m.put("os", s"${System.getProperty("os.name")} ${System.getProperty("os.version")} ${System.getProperty("os.arch")}")
    if (System.getProperty("os.name", "").startsWith("Mac")) {
      m.put("cpu", sysctl("machdep.cpu.brand_string"))
      // Apple silicon: perflevel0 is the fastest cores, perflevel1 the next
      for (l <- 0 to 2; n = sysctl(s"hw.perflevel$l.physicalcpu"); if n.nonEmpty)
        m.put(s"cores.$l", s"$n ${sysctl(s"hw.perflevel$l.name")}".trim)
    }
    m
  }

  private final class LongBuf {
    private[this] var a = new Array[Long](1024)
    private[this] var n = 0
    private def room(k: Int): Unit = if (n + k > a.length) a = java.util.Arrays.copyOf(a, a.length * 2)
    def add5(x0: Long, x1: Long, x2: Long, x3: Long, x4: Long): Unit = {
      room(5); a(n) = x0; a(n + 1) = x1; a(n + 2) = x2; a(n + 3) = x3; a(n + 4) = x4; n += 5
    }
    def add6(x0: Long, x1: Long, x2: Long, x3: Long, x4: Long, x5: Long): Unit = {
      room(6); a(n) = x0; a(n + 1) = x1; a(n + 2) = x2; a(n + 3) = x3; a(n + 4) = x4; a(n + 5) = x5; n += 6
    }
    def foreach(stride: Int)(f: (Array[Long], Int) => Unit): Unit = { var i = 0; while (i < n) { f(a, i); i += stride } }
  }
}
