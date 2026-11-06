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

package scala.tools.nsc
package typechecker

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.collection.mutable
import scala.reflect.internal.util.CodeAction
import scala.reflect.internal.util.{ParallelTyperTimeline, PerThreadTimes}
import scala.tools.nsc.reporters.FilteringReporter

/** `-Yparallel-typer` (a spike): the typer phase typechecks its units on several threads over one `Global`.
 *
 *  Parser, namer and packageobjects run serially as before. Then, on the main thread: the lazy vals of `definitions`
 *  and `runDefinitions` are forced (their initialisers complete symbols under the lazy val's monitor, a deadlock
 *  against the completion lock), and by default the signature of every member defined in the sources is completed
 *  ("signatures first"): a Namer completer types trees of its own unit with its own unit's contexts and per-unit
 *  maps, so it must not run on another unit's thread while that unit's thread types the same state. Then the bodies
 *  are typed with a unit per task, largest first, on a pool of workers, each with its own phase, current unit and
 *  scratch state. Every completion that still happens runs under the completion lock (`SymbolTable.completionLock`).
 *  The diagnostics are buffered and issued after the last unit in a fixed order (by file, then position, then
 *  arrival). The phases after the typer run on the main thread as before.
 *
 *  System properties, for measurement:
 *   - `scalac.parallelTyper.lazySignatures`: no signatures-first pre-pass (completers run where they are reached).
 *   - `scalac.parallelTyper.oneThread`: the parallel path on one worker, to tell order effects from concurrency.
 *   - `scalac.parallelTyper.threads=N`: the number of workers (else `-Yparallel-typer-threads`, else the processors).
 *   - `scalac.parallelTyper.log`: a summary on stderr. `scalac.parallelTyper.timeline=<file>`: a JSON timeline.
 *   - `scalac.parallelTyper.watchdog=<seconds>`: dump the threads when the phase runs longer (default 120).
 */
trait ParallelTyper {
  self: Analyzer =>

  import global._

  /** Whether signatures are completed under a claim per symbol: the default, with only the class headers serial.
   *  `-Dscalac.parallelTyper.serialSignatures` restores the serial pre-pass under the one completion lock. */
  lazy val parallelSignatures: Boolean = scala.reflect.internal.util.CompletionLock.parallelSignatures
  /** `-Dscalac.parallelTyper.eagerImports`: complete every import with the class headers (see `completeSignatures`). */
  private lazy val eagerImports: Boolean = System.getProperty("scalac.parallelTyper.eagerImports") != null

  /** Is the typer phase running its workers? (`compileLate` is not supported then.) */
  @volatile var parallelTyperActive = false

  private def prop(name: String): Option[String] = Option(System.getProperty("scalac.parallelTyper." + name))
  private def flag(name: String): Boolean = prop(name).exists(v => v.isEmpty || v.toBoolean)

  private def unsupported(): List[String] = {
    val s = settings
    List(
      (s.YmacroAnnotations.value, "-Ymacro-annotations"),
      (hasAnalyzerOrMacroPlugins, "analyzer or macro plugins"),
      (s.Ytyperdebug.value, "-Ytyper-debug"),
      (s.YprofileEnabled.value, "-Yprofile-enabled"),
      (s.areStatisticsEnabled, "-Ystatistics"),
      (s.check.value.nonEmpty, "-Ycheck"),
      (s.log.isSetByUser, "-Ylog"),
      (s.isScaladoc, "scaladoc"),
      (s.Vimplicits.value, "-Vimplicits"),     // splain's `ImplicitErrors` is one stack for all threads
      (s.cyclic.value, "-Vcyclic"),            // the locking trace is one buffer for all threads
      (s.VtypeDiffs.value, "-Vtype-diffs"),     // splain's format caches are shared maps
    ).collect { case (true, what) => what }
  }

  def typeCheckInParallel(typerPhase: Phase, units: List[CompilationUnit], applyPhase: CompilationUnit => Unit): Unit = {
    assert(isConcurrentCompiler, "-Yparallel-typer: the universe was not constructed for it")
    val bad = unsupported()
    if (bad.nonEmpty) {
      globalError(s"-Yparallel-typer does not support ${bad.mkString(", ")}")
      return
    }
    val log = flag("log") || settings.verbose.value
    def logf(msg: => String): Unit = if (log) Console.err.println("[parallel-typer] " + msg)

    val nThreads = {
      val n =
        if (flag("oneThread")) 1
        else prop("threads").map(_.toInt).getOrElse {
          val s = settings.YparallelTyperThreads.value
          if (s > 0) s else math.min(Runtime.getRuntime.availableProcessors, 8)
        }
      math.max(1, math.min(n, units.length))
    }
    // The lazy vals of the cake are forced up front only for the lock order between their monitors and the completion
    // lock, which needs two workers to deadlock: one worker skips the cost (≈8% of a one-worker run of re2s, and all of
    // its extra allocation). `noForceLazyVals` skips it on any number, for experiments.
    val needForceLazyVals = nThreads > 1 && !flag("noForceLazyVals")
    // largest first, so that the last units to start are small
    // (`-Dscalac.parallelTyper.order=source`: in source order, as the serial typer)
    val ordered = if (prop("order").contains("source")) units else units.sortBy(u => -u.source.content.length)

    val t0 = System.nanoTime()
    val signatures =
      if (flag("lazySignatures")) 0
      else if (parallelSignatures && !flag("poolSignatures")) {
        // Only the class headers serially. A member's signature is completed by the first thread that asks for it, under
        // the symbol's claim, and its annotations by its own unit's task (`typedTemplate`, `typedValDef`, `typedDefDef`
        // and `typedTypeDef` force them), so the typer has one pool step and one tail.
        var n = 0
        units.foreach(u => n += ParallelTyperTimeline.span("class headers", u.spanLabel)(completeSignatures(u, null, classesOnly = true))) // xray
        logf(f"class headers serially in ${(System.nanoTime() - t0) / 1e6}%.1f ms; members and annotations left to the body tasks")
        n
      }
      else if (parallelSignatures) {
        // -Dscalac.parallelTyper.poolSignatures, for comparison: the class headers serially, then the members' signatures
        // as a pool step of their own, then the annotations serially.
        var n = 0
        val completed = new ConcurrentLinkedQueue[(CompilationUnit, Symbol)]
        val count = new AtomicInteger
        units.foreach(u => n += ParallelTyperTimeline.span("class headers", u.spanLabel)(completeSignatures(u, null, classesOnly = true))) // xray
        val headersOnly = System.nanoTime()
        // The lazy vals of `definitions` before the members run on several threads: one forced from a completion holds
        // the object's monitor while it waits for a claim, and another thread holding that claim waits for the monitor
        // (a deadlock the claims cannot see). After the headers, so that what they name is ready (I3).
        if (needForceLazyVals) ParallelTyperTimeline.span("lazy vals", "definitions, runDefinitions, the cake")(forceLazyVals()) // xray
        val headersDone = System.nanoTime()
        onPool(typerPhase, ordered, nThreads, "signatures") { u =>
          val buf = mutable.ArrayBuffer.empty[(CompilationUnit, Symbol)]
          count.addAndGet(completeSignatures(u, buf))
          buf.foreach(completed.add)
        }
        val membersDone = System.nanoTime()
        ParallelTyperTimeline.span("annotations", "all symbols")(forceAnnotations(completed.asScala.toList)) // xray
        logf(f"class headers serially in ${(headersOnly - t0) / 1e6}%.1f ms, lazy vals in ${(headersDone - headersOnly) / 1e6}%.1f ms, member signatures on $nThreads threads in ${(membersDone - headersDone) / 1e6}%.1f ms, annotations serially in ${(System.nanoTime() - membersDone) / 1e6}%.1f ms")
        n + count.get
      }
      else {
        var n = 0
        val completed = mutable.ArrayBuffer.empty[(CompilationUnit, Symbol)]
        units.foreach(u => n += ParallelTyperTimeline.span("signatures", u.spanLabel)(completeSignatures(u, completed))) // xray
        // Then their annotations, in a pass of their own: an annotation is lazy so that its arguments can refer to
        // symbols still being completed. A thunk types the annotation with its symbol's context, in place, so it
        // must not run on another unit's thread; and outside `info`, the lock would not cover it (it is taken
        // before the lazy val's monitor, `LazyAnnotationInfo.forcedInfo`, for the ones that are left).
        ParallelTyperTimeline.span("annotations", "all symbols")(forceAnnotations(completed)) // xray
        n
      }
    val t1 = System.nanoTime()
    logf(f"$signatures signatures completed serially in ${(t1 - t0) / 1e6}%.1f ms")
    // After the signatures: a lazy val of `definitions` can name a symbol defined in the sources (compiling the
    // library), and forcing it before that symbol's unit is ready meets a cycle, which leaves a cache stamped in
    // progress (`CompoundType.baseClasses`) for the rest of the run.
    if (needForceLazyVals) ParallelTyperTimeline.span("lazy vals", "definitions, runDefinitions, the cake")(forceLazyVals()) // xray
    val t2 = System.nanoTime()
    logf(f"lazy vals forced in ${(t2 - t1) / 1e6}%.1f ms")

    val bufferingReporter = new BufferingReporter(reporter)
    val savedReporter = reporter
    reporter = bufferingReporter
    val lock = completionLock
    lock.reset()
    val timeline = prop("timeline").map(_ => new Timeline)

    val queue = new ConcurrentLinkedQueue[CompilationUnit](ordered.asJava)
    val failure = new AtomicReference[Throwable]()
    val done = new AtomicInteger()
    val body: Runnable = () => {
        try {
          phase = typerPhase
          var unit = queue.poll()
          while (unit != null && failure.get == null) {
            val s0 = System.nanoTime()
            val c0 = Timeline.cpuNow()
            val times = PerThreadTimes.current
            val w0 = times.waitNanos
            val h0 = times.holdNanos
            val trees0 = nodeCount
            try applyPhase(unit)
            finally {
              unitWeights.put(unit, nodeCount - trees0)
              patternWeights.put(unit, patternsIn(unit.body))
              undoLog.clear()
              scratch.memberTypes.clear() // only needed within a typing; keeps this from holding every method of the run
              timeline.foreach(_.add(unit.source.file.name, s0, System.nanoTime(), Timeline.cpuNow() - c0,
                times.waitNanos - w0, times.holdNanos - h0))
            }
            done.incrementAndGet()
            unit = queue.poll()
          }
          if (ParallelTyperTimeline.current ne null) ParallelTyperTimeline.current.idle() // xray
        } catch {
          case ex: Throwable => failure.compareAndSet(null, ex)
        } finally {
          phase = NoPhase
        }
    }
    val watchdog = newWatchdog(prop("watchdog").map(_.toInt).getOrElse(120))
    parallelTyperActive = true
    currentRun.reporting.deferFeatureExplanations = true
    try {
      watchdog.start()
      workers.start(nThreads, body).await()
    } finally {
      parallelTyperActive = false
      currentRun.reporting.deferFeatureExplanations = false
      watchdog.interrupt()
      reporter = savedReporter
      bufferingReporter.issueBuffered()
    }
    val t3 = System.nanoTime()
    logf(f"${units.length} units typed in ${(t3 - t2) / 1e6}%.1f ms on $nThreads threads; ${lock.summary}${lock.siteSummary}")
    timeline.foreach(tl => tl.write(prop("timeline").get, t0, t1, t2, t3, nThreads))
    failure.get match {
      case null =>
      case ex   => closeWorkers(); throw ex
    }
  }

  /** Can the phases that run a unit per task do so with the settings of this run? */
  def parallelPhasesSupported: Boolean = unsupported().isEmpty

  /** `task` on each of `units` on the parallel typer's pool, largest first, for a phase other than the typer.
   *  With `unitOrder`, the diagnostics are issued in the order of `units` and, within a unit, of arrival, which is
   *  the serial order of a phase whose task reports only while it works on its own unit; a task may then ask the
   *  reporter what the serial phase would have reported before it (`UnitOrderReporter`); the units are then also
   *  started in that order, unless `largestFirst`: a phase whose tasks do not ask starts its largest units first
   *  all the same, so that a large unit started late is not left running alone at the end (in the compiler's sources,
   *  `Typers.scala` was, in every phase after the typer). `meanwhile` runs on the calling thread while the workers
   *  run, and is given a test of whether any of them still does.
   */
  def parallelPhase(phase0: Phase, units: List[CompilationUnit], name: String, unitOrder: Boolean = false,
                    meanwhile: (() => Boolean) => Unit = null, largestFirst: Boolean = false)(task: CompilationUnit => Unit): Unit = {
    val n =
      if (flag("oneThread")) 1
      else prop("threads").map(_.toInt).getOrElse {
        val s = settings.YparallelTyperThreads.value
        if (s > 0) s else math.min(Runtime.getRuntime.availableProcessors, 8)
      }
    val nThreads = math.max(1, math.min(n, units.length))
    def bySize = units.sortBy(u => -weightOf(u, name))
    if (unitOrder) onPool(phase0, if (largestFirst) bySize else units, nThreads, name, units, meanwhile)(task)
    else onPool(phase0, bySize, nThreads, name, null, meanwhile)(task)
  }

  /** `task` on each of `units`, a unit per task on `nThreads` workers at `phase`, the diagnostics buffered and issued
   *  in order afterwards. */
  private def onPool(phase0: Phase, units: List[CompilationUnit], nThreads: Int, name: String, unitOrder: List[CompilationUnit] = null,
                     meanwhile: (() => Boolean) => Unit = null)(task: CompilationUnit => Unit): Unit = {
    val bufferingReporter = new BufferingReporter(reporter, unitOrder)
    val savedReporter = reporter
    reporter = bufferingReporter
    val queue = new ConcurrentLinkedQueue[CompilationUnit](units.asJava)
    val failure = new AtomicReference[Throwable]()
    // units whose task asked what the serial phase would have reported before it (`RetryInUnitOrder`)
    val retry = new ConcurrentLinkedQueue[CompilationUnit]()
    val body: Runnable = () => {
        try {
          phase = phase0
          var unit = queue.poll()
          while (unit != null && failure.get == null) {
            val saved = currentRun.currentUnit
            currentRun.currentUnit = unit
            try ParallelTyperTimeline.span(name, unit.spanLabel)(task(unit)) // xray
            catch { case ast.parser.RetryInUnitOrder if unitOrder ne null => retry.add(unit) }
            finally { currentRun.currentUnit = saved; undoLog.clear() }
            unit = queue.poll()
          }
        } catch {
          case ex: Throwable => failure.compareAndSet(null, ex)
        } finally {
          phase = NoPhase
        }
    }
    val watchdog = newWatchdog(prop("watchdog").map(_.toInt).getOrElse(120))
    parallelTyperActive = true
    currentRun.reporting.deferFeatureExplanations = true
    try {
      watchdog.start()
      val running = workers.start(nThreads, body)
      if (meanwhile ne null) meanwhile(() => running.getCount > 0)
      running.await()
      // Again, here, in the order of the units, each with the diagnostics of the units before it at hand.
      if (failure.get == null && !retry.isEmpty) {
        val again = retry.asScala.toSet
        if (flag("log")) Console.err.println(s"[parallel-typer] $name: ${again.size} units again, in the order of the units")
        val saved = currentRun.currentUnit
        try unitOrder.foreach { unit =>
          if (again(unit)) {
            bufferingReporter.retrying(unit)
            currentRun.currentUnit = unit
            ParallelTyperTimeline.span(name, unit.spanLabel)(task(unit)) // xray
          }
        } finally currentRun.currentUnit = saved
      }
    } finally {
      parallelTyperActive = false
      currentRun.reporting.deferFeatureExplanations = false
      watchdog.interrupt()
      reporter = savedReporter
      bufferingReporter.issueBuffered()
    }
    failure.get match {
      case null =>
      case ex   => closeWorkers(); throw ex
    }
  }

  /** The worker threads of a run, `scalac-worker-1` and up. Every step that runs a unit per task runs on them: they
   *  are made when a step first needs them and kept until the run ends (`closeWorkers`), where each step used to make
   *  threads of its own. A worker's scratch state, with what it caches, then lasts for the run, and a timeline has a
   *  lane per worker. A worker refers to this compiler, so workers must not outlive the run.
   */
  private object workers {
    private[this] val Stop: Runnable = () => ()
    private final class Worker(i: Int) {
      private[this] val jobs = new java.util.concurrent.LinkedBlockingQueue[Runnable]
      private[this] val thread = new scala.reflect.internal.util.ScratchThread(null, () => {
        var job = jobs.take()
        while (job ne Stop) {
          try job.run() catch { case _: Throwable => } // a job reports its own failure
          job = jobs.take()
        }
      }, s"scalac-worker-$i", 1L << 28)
      thread.setDaemon(true)
      thread.start()
      def submit(job: Runnable): Unit = jobs.put(job)
    }
    private[this] val all = mutable.ArrayBuffer.empty[Worker]
    /** `body` on `n` workers at once; the latch opens when all of them are done with it. */
    def start(n: Int, body: Runnable): java.util.concurrent.CountDownLatch = synchronized {
      while (all.length < n) all += new Worker(all.length + 1)
      val done = new java.util.concurrent.CountDownLatch(n)
      all.iterator.take(n).foreach(_.submit(() => try body.run() finally done.countDown()))
      done
    }
    def close(): Unit = synchronized { all.foreach(_.submit(Stop)); all.clear() }
  }
  /** End the worker threads of this run, if it had any. */
  def closeWorkers(): Unit = { workers.close(); unitWeights.clear(); patternWeights.clear() }

  /** How many trees the typer made for each unit of this run: what the phases after it take as the unit's weight, to
   *  start the heaviest first. The length of the source, which orders the typer's own units, is a poor guide to
   *  what a typed unit costs later: a short file of quasiquotes or of matches is a large tree (in the compiler's
   *  sources `Extractors.scala` was started late by its length and left patmat running on it alone). */
  private[this] val unitWeights = new java.util.concurrent.ConcurrentHashMap[CompilationUnit, Integer]
  /** For patmat, the trees in a unit's patterns, counted when it is typed: what that phase costs follows the
   *  patterns, and a file can be all patterns (`Extractors.scala` again, which by its trees alone still started 84th). */
  private[this] val patternWeights = new java.util.concurrent.ConcurrentHashMap[CompilationUnit, Integer]
  private def patternsIn(tree: Tree): Int = {
    var n = 0
    new Traverser {
      override def traverse(t: Tree): Unit = t match {
        case CaseDef(pat, guard, body) => pat.foreach(_ => n += 1); traverse(guard); traverse(body)
        case _                         => super.traverse(t)
      }
    }.traverse(tree)
    n
  }
  private def weightOf(unit: CompilationUnit, phaseName: String): Int = {
    val w = (if (phaseName == "patmat") patternWeights else unitWeights).get(unit)
    if (w ne null) w.intValue else unit.source.content.length
  }

  /** The thread dump of a phase that runs longer than `seconds`, and again every `seconds`. */
  private def newWatchdog(seconds: Int): Thread = {
    val t = new Thread(() => {
      try {
        while (true) {
          Thread.sleep(seconds * 1000L)
          val infos = java.lang.management.ManagementFactory.getThreadMXBean.dumpAllThreads(true, true)
          val sb = new StringBuilder(s"[parallel-typer] still running after $seconds s; ${completionLock.summary}\n")
          infos.foreach { ti =>
            sb.append(ti.toString.linesIterator.take(1).mkString).append('\n')
            ti.getStackTrace.take(60).foreach(f => sb.append("\tat ").append(f).append('\n'))
          }
          Console.err.println(sb)
        }
      } catch { case _: InterruptedException => () }
    }, "scalac-typer-watchdog")
    t.setDaemon(true)
    t
  }

  /** Force the lazy vals of `definitions` and `runDefinitions` (and their nested objects) on this thread: their
   *  initialisers complete symbols while they hold the monitor of the enclosing object, and a worker that held the
   *  completion lock while it waited for that monitor would deadlock with one that held the monitor while it waited
   *  for the lock. Optional classes are missing in some configurations: such a failure is ignored here (as it is
   *  in use, where the lazy val is retried).
   */
  private def forceLazyVals(): Unit = {
    val seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[AnyRef, java.lang.Boolean])
    def force(obj: AnyRef, depth: Int): Unit = forceLazyValsOf(obj, depth, seen)
    force(definitions, 0)
    force(currentRun.runDefinitions, 0)
    // private lazy vals of the cake that complete symbols, e.g. `TypeConstraints.numericLoBound`
    for (m <- global.getClass.getMethods ++ global.getClass.getDeclaredMethods
         if m.getParameterCount == 0 && m.getName.contains("$reflect$internal$") && !m.getName.endsWith("$lzycompute")) {
      try { m.setAccessible(true); m.invoke(global) } catch { case _: Throwable => () }
    }
  }

  /** The lazy vals of a phase's component, before its workers start: as `forceLazyVals`. */
  def forceLazyValsOf(component: SubComponent): Unit = if (!flag("oneThread")) forceLazyValsOf(component: AnyRef, 0, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]))

  def forceLazyValsOfObject(obj: AnyRef): Unit = if (!flag("oneThread")) forceLazyValsOf(obj, 0, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]))

  private def forceLazyValsOf(obj: AnyRef, depth: Int, seen: java.util.Set[AnyRef]): Unit =
    if (obj != null && depth < 3 && seen.add(obj)) {
      var cls: Class[_] = obj.getClass
      while (cls != null && cls != classOf[Object]) {
        val methods = cls.getDeclaredMethods
        val names = methods.iterator.map(_.getName).toSet
        val fields = cls.getDeclaredFields.iterator.map(_.getName).toSet
        for (m <- methods if m.getParameterCount == 0 && !m.getName.endsWith("$lzycompute")) {
          val isLazy = names(m.getName + "$lzycompute")
          val isModule = fields(m.getName + "$module")
          if (isLazy || isModule) {
            try {
              m.setAccessible(true)
              val v = m.invoke(obj)
              if (isModule) forceLazyValsOf(v.asInstanceOf[AnyRef], depth + 1, seen)
            } catch { case _: Throwable => () }
          }
        }
        cls = cls.getSuperclass
      }
    }

  /** Complete the signature of every member that `unit` defines, outside method bodies: the classes, objects, their
   *  type parameters, members (those of the trees and the synthetic ones in their scopes), self types and
   *  annotations. Returns the number of symbols completed. A type error that escapes a completion is reported here,
   *  at the symbol, where the serial typer would report it at the tree that first needed the symbol.
   */
  private def forceAnnotations(syms: Iterable[(CompilationUnit, Symbol)]): Unit = {
    val savedUnit = currentRun.currentUnit
    try syms.foreach { case (unit, sym) =>
      currentRun.currentUnit = unit
      try sym.annotations.foreach(_.completeInfo())
      catch { case ex: TypeError => reporter.error(sym.pos, ex.getMessage) }
    }
    finally currentRun.currentUnit = savedUnit
  }

  private def completeSignatures(unit: CompilationUnit, completed: mutable.ArrayBuffer[(CompilationUnit, Symbol)], classesOnly: Boolean = false): Int = {
    var n = 0
    val seen = mutable.HashSet[Symbol]()
    val savedUnit = currentRun.currentUnit
    currentRun.currentUnit = unit
    def complete(sym: Symbol): Unit =
      if (sym != null && sym != NoSymbol && seen.add(sym)) {
        n += 1
        try {
          sym.initialize
          sym.typeParams.foreach(complete)
          if (sym.isMethod) sym.paramss.foreach(_.foreach(complete))
          if (completed ne null) completed += ((unit, sym))
          if (sym.isModule) complete(sym.moduleClass)
          if (sym.isClass && !classesOnly) {
            sym.typeOfThis
            var decls = Set.empty[Symbol]
            var more = true
            while (more) { // completing a member can enter more (default getters, case class methods)
              val ds = sym.info.decls.toList.filterNot(decls)
              decls ++= ds
              ds.foreach(complete)
              more = ds.nonEmpty
            }
          }
        } catch {
          case ex: TypeError => reporter.error(sym.pos, ex.getMessage)
        }
      }
    // The names of the modules among the unit's root imports (`Predef`): an import of one of them at the top level
    // excludes it from the unit's root imports from the moment the import is completed (`Contexts.registerImport`).
    lazy val rootModuleNames: Set[Name] = rootImports(unit).filterNot(_.hasPackageFlag).map(_.name: Name).toSet
    // Could completing this import exclude a root import? Only one at the top level whose qualifier ends in the name
    // of such a module, or is an identifier, which an earlier renaming import may have made a name for it.
    def mayExcludeRootImport(imp: Import): Boolean = imp.expr match {
      case Select(_, name) => rootModuleNames(name.toTermName)
      case _               => true
    }
    def traverse(tree: Tree, topLevel: Boolean): Unit = tree match {
      case PackageDef(_, stats) => stats.foreach(traverse(_, topLevel))
      case cd: ClassDef         => complete(cd.symbol); cd.impl.body.foreach(traverse(_, topLevel = false))
      case md: ModuleDef        => complete(md.symbol); md.impl.body.foreach(traverse(_, topLevel = false))
      case md: MemberDef        => if (!classesOnly) complete(md.symbol)
      // In tree order, as the typer types them, the imports that may exclude a root import. With `classesOnly` the
      // others are left to the thread that first looks a name up through them, as in the serial compiler: completing
      // an import loads the classes it names, and all of them up front was four fifths of the class headers step
      // (`-Dscalac.parallelTyper.eagerImports` completes them all here, as before).
      case imp: Import          => if (!classesOnly || eagerImports || (topLevel && mayExcludeRootImport(imp))) complete(imp.symbol)
      case _                    =>
    }
    try {
      traverse(unit.body, topLevel = true)
      if (!classesOnly) unit.synthetics.keys.toList.foreach(complete)
    } finally currentRun.currentUnit = savedUnit
    n
  }

  /** The run's reporter while the workers run: every report is under its monitor; the diagnostics are kept and then
   *  issued in a fixed order (by file, then position, then arrival). Filtering (duplicates at a position, -Xmaxerrs)
   *  happens when they are issued, in the underlying reporter. The messages arrive rendered.
   *
   *  With `unitOrder` (a phase whose task reports only on its own unit): issued in the order of those units and,
   *  within a unit, of arrival, which is the order of the serial phase.
   */
  private final class BufferingReporter(val underlying: FilteringReporter, unitOrder: List[CompilationUnit] = null) extends FilteringReporter with ast.parser.UnitOrderReporter {
    private final case class Entry(pos: Position, msg: String, severity: Severity, actions: List[CodeAction], seq: Int, unit: Int)
    private[this] val buffer = mutable.ArrayBuffer.empty[Entry]
    private[this] var seqs = 0
    private[this] val unitIndex: java.util.IdentityHashMap[CompilationUnit, Integer] =
      if (unitOrder eq null) null
      else { val m = new java.util.IdentityHashMap[CompilationUnit, Integer]; unitOrder.foreach(u => m.put(u, m.size)); m }
    // a report outside any of the units (there is none in the parser) goes last
    private def currentIndex: Int =
      if (unitIndex eq null) 0 else { val i = unitIndex.get(currentRun.currentUnit); if (i eq null) Int.MaxValue else i }
    @volatile private[this] var retryingInOrder = false

    /** `unit` is about to be processed again, on the driver thread, after the units before it. */
    def retrying(unit: CompilationUnit): Unit = synchronized {
      retryingInOrder = true
      val i = unitIndex.get(unit)
      buffer.filterInPlace(_.unit != i)
    }
    def isRetryInUnitOrder: Boolean = retryingInOrder
    def hasErrorsInUnitOrder: Boolean =
      if (unitIndex eq null) hasErrors
      else if (!retryingInOrder) throw ast.parser.RetryInUnitOrder
      else synchronized {
        val i = currentIndex
        underlying.hasErrors || buffer.exists(e => e.unit <= i && e.severity == scala.reflect.internal.Reporter.ERROR)
      }
    def settings: Settings = underlying.settings
    override def filter(pos: Position, msg: String, severity: Severity): Int = 0 // Display
    override def increment(severity: Severity): Unit = synchronized(super.increment(severity))
    override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit =
      synchronized { seqs += 1; buffer += Entry(pos, msg, severity, actions, seqs, currentIndex) }
    override def hasErrors: Boolean = synchronized(super.hasErrors) || underlying.hasErrors
    override def cancelled: Boolean = underlying.cancelled
    override def cancelled_=(b: Boolean): Unit = underlying.cancelled = b
    override def comment(pos: Position, msg: String): Unit = underlying.comment(pos, msg)
    override def flush(): Unit = ()

    def issueBuffered(): Unit = synchronized {
      val ordered =
        if (unitIndex ne null) buffer.sortBy(e => (e.unit, e.seq))
        else buffer.sortBy { e =>
          val p = e.pos
          val defined = p != null && p.isDefined
          (if (defined) p.source.file.path else "", if (defined) p.point else -1, e.seq)
        }
      buffer.clear()
      ordered.foreach { e =>
        // a feature is explained at its first warning in this order, not in the order of arrival
        val msg = currentRun.reporting.explainFeature(e.msg)
        e.severity match {
          case scala.reflect.internal.Reporter.ERROR   => underlying.error(e.pos, msg, e.actions)
          case scala.reflect.internal.Reporter.WARNING => underlying.warning(e.pos, msg, e.actions)
          case _                         => underlying.echo(e.pos, msg, e.actions)
        }
      }
    }
  }

  /** What each worker did: a span per unit with its wall and CPU time, and its time waiting for and holding the
   *  completion lock. Written as JSON, with the serial steps before it. */
  private final class Timeline {
    private final case class Span(thread: String, unit: String, start: Long, end: Long, cpu: Long, lockWait: Long, lockHold: Long)
    private[this] val spans = new ConcurrentLinkedQueue[Span]()
    def add(unit: String, start: Long, end: Long, cpu: Long, lockWait: Long, lockHold: Long): Unit =
      spans.add(Span(Thread.currentThread.getName, unit, start, end, cpu, lockWait, lockHold))
    def write(file: String, t0: Long, t1: Long, t2: Long, t3: Long, threads: Int): Unit = {
      def ms(ns: Long) = f"${ns / 1e6}%.3f"
      def str(s: String) = "\"" + s.flatMap { case '"' => "\\\""; case '\\' => "\\\\"; case c => c.toString } + "\""
      val ss = spans.asScala.toList.sortBy(_.start)
      val json = new StringBuilder
      json.append(s"""{"threads":$threads,"signaturesMs":${ms(t1 - t0)},"lazyValsMs":${ms(t2 - t1)},"bodiesMs":${ms(t3 - t2)},\n"spans":[\n""")
      json.append(ss.map(s => s"[${str(s.thread)},${str(s.unit)},${ms(s.start - t2)},${ms(s.end - t2)},${ms(s.cpu)},${ms(s.lockWait)},${ms(s.lockHold)}]").mkString(",\n"))
      json.append("]}\n")
      java.nio.file.Files.write(java.nio.file.Paths.get(file), json.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }
  }
  private object Timeline {
    private[this] val bean = java.lang.management.ManagementFactory.getThreadMXBean
    def cpuNow(): Long = bean.getCurrentThreadCpuTime
  }

  private implicit class AsJava[T](xs: List[T]) {
    def asJava: java.util.Collection[T] = { val l = new java.util.ArrayList[T](xs.length); xs.foreach(l.add); l }
  }
  private implicit class AsScala[T](q: ConcurrentLinkedQueue[T]) {
    def asScala: Iterator[T] = { val it = q.iterator; Iterator.continually(it).takeWhile(_.hasNext).map(_.next()) }
  }
}
