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

package scala.tools.nsc.profile

import java.io.{FileWriter, PrintWriter}
import java.lang.management.ManagementFactory
import java.nio.file.Paths
import java.util.ServiceLoader
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import scala.annotation.{nowarn, unused}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.ChromeTrace
import scala.reflect.internal.NoPhase
import scala.reflect.io.AbstractFile
import scala.tools.nsc.{Global, Phase, Settings}

object Profiler {
  def apply(settings: Settings): Profiler = apply(settings, null)

  /** The run's profiler: `-Yprofile-enabled` (`RealProfiler`), the cost per definition alone (`CostProfiler`), or
   *  nothing (`NoOpProfiler`, whose hooks do nothing). */
  def apply(global: Global): Profiler = apply(global.settings, DefCosts(global))

  private def apply(settings: Settings, costs: DefCosts): Profiler =
    if (!settings.YprofileEnabled.value) {
      if (costs == null) NoOpProfiler else new CostProfiler(costs)
    } else {
      val reporter = settings.YprofileDestination.value match {
        case _ if !settings.YprofileDestination.isSetByUser => NoOpProfileReporter
        case "-" => ConsoleProfileReporter
        case path => new StreamProfileReporter(new PrintWriter(new FileWriter(path, true)))
      }
      new RealProfiler(reporter, settings, costs)
    }

  private[profile] val emptySnap = ProfileSnap(0, "", 0, 0, 0, 0, 0, 0, 0, 0)
}
case class GcEventData(pool:String, reportTimeNs: Long, gcStartMillis:Long, gcEndMillis:Long, durationMillis: Long, name:String, action:String, cause:String, threads:Long) {
  val endNanos = System.nanoTime()
}

case class ProfileSnap(threadId: Long, threadName: String, snapTimeNanos : Long,
                       idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long,
                       allocatedBytes:Long, heapBytes:Long, totalClassesLoaded: Long, totalJITCompilationTime: Long) {
  def updateHeap(heapBytes:Long) = {
    copy(heapBytes = heapBytes)
  }
}
case class ProfileRange(start: ProfileSnap, end:ProfileSnap, phase:Phase, purpose:String, taskCount:Int, thread:Thread) {
  def allocatedBytes = end.allocatedBytes - start.allocatedBytes

  def userNs = end.userTimeNanos - start.userTimeNanos

  def cpuNs = end.cpuTimeNanos - start.cpuTimeNanos

  def idleNs = end.idleTimeNanos - start.idleTimeNanos

  def runNs = end.snapTimeNanos - start.snapTimeNanos


  private def toMillis(ns: Long) = ns / 1000000.0D

  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D


  def wallClockTimeMillis = toMillis(end.snapTimeNanos - start.snapTimeNanos)

  def idleTimeMillis = toMillis(end.idleTimeNanos - start.idleTimeNanos)

  def cpuTimeMillis = toMillis(end.cpuTimeNanos - start.cpuTimeNanos)

  def userTimeMillis = toMillis(end.userTimeNanos - start.userTimeNanos)

  def allocatedMB = toMegaBytes(end.allocatedBytes - start.allocatedBytes)

  def retainedHeapMB = toMegaBytes(end.heapBytes - start.heapBytes)
}

sealed abstract class Profiler {

  def finished(): Unit

  def beforePhase(phase: Phase): ProfileSnap

  def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit

  def beforeUnit(phase: Phase, file: AbstractFile): Unit

  def afterUnit(phase: Phase, file: AbstractFile): Unit

  def beforeTypedImplDef(sym: Global#Symbol): Unit = ()
  def afterTypedImplDef(sym: Global#Symbol): Unit = ()

  def beforeImplicitSearch(pt: Global#Type): Unit = ()
  def afterImplicitSearch(pt: Global#Type): Unit = ()

  def beforeMacroExpansion(macroSym: Global#Symbol): Unit = ()
  def afterMacroExpansion(macroSym: Global#Symbol): Unit = ()

  def beforeCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = ()
  def afterCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = ()

  /** Completing `sym` from source (a Namer type completer for `tree`, its definition); only for the cost per definition */
  def beforeNamerCompletion(sym: Global#Symbol, tree: Global#Tree): Unit = ()
  def afterNamerCompletion(sym: Global#Symbol): Unit = ()
}
private [profile] object NoOpProfiler extends Profiler {

  override def beforePhase(phase: Phase): ProfileSnap = Profiler.emptySnap

  override def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit = ()

  override def beforeUnit(phase: Phase, file: AbstractFile): Unit = ()
  override def afterUnit(phase: Phase, file: AbstractFile): Unit = ()
  override def finished(): Unit = ()
}

/** Only the cost per definition (`-Yprofile-defs`, `-Yprofile-defs-listener`), without the rest of `-Yprofile-enabled` */
private [profile] final class CostProfiler(costs: DefCosts) extends Profiler {
  override def finished(): Unit = costs.finished()

  override def beforePhase(phase: Phase): ProfileSnap = { costs.enter(CostKind.Phase, phase, phase); Profiler.emptySnap }
  override def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit = costs.exit(CostKind.Phase, phase)

  override def beforeUnit(phase: Phase, file: AbstractFile): Unit = costs.enter(CostKind.Unit, file, phase)
  override def afterUnit(phase: Phase, file: AbstractFile): Unit = costs.exit(CostKind.Unit, file)

  override def beforeTypedImplDef(sym: Global#Symbol): Unit = costs.enter(CostKind.TypedDef, sym, NoPhase)
  override def afterTypedImplDef(sym: Global#Symbol): Unit = costs.exit(CostKind.TypedDef, sym)

  override def beforeImplicitSearch(pt: Global#Type): Unit = costs.enter(CostKind.Implicit, null, NoPhase)
  override def afterImplicitSearch(pt: Global#Type): Unit = costs.exit(CostKind.Implicit, null)

  override def beforeMacroExpansion(macroSym: Global#Symbol): Unit = costs.enter(CostKind.Macro, null, NoPhase)
  override def afterMacroExpansion(macroSym: Global#Symbol): Unit = costs.exit(CostKind.Macro, null)

  override def beforeCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = costs.enter(CostKind.Load, root, NoPhase)
  override def afterCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = costs.exit(CostKind.Load, root)

  override def beforeNamerCompletion(sym: Global#Symbol, tree: Global#Tree): Unit = {
    costs.definedBy(sym, tree)
    costs.enter(CostKind.Completion, sym, NoPhase)
  }
  override def afterNamerCompletion(sym: Global#Symbol): Unit = costs.exit(CostKind.Completion, sym)
}

private [profile] object RealProfiler {
  import scala.jdk.CollectionConverters._
  val runtimeMx = ManagementFactory.getRuntimeMXBean
  val memoryMx = ManagementFactory.getMemoryMXBean
  val gcMx = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  val classLoaderMx = ManagementFactory.getClassLoadingMXBean
  val compileMx = ManagementFactory.getCompilationMXBean
  val threadMx = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
  lazy val allPlugins = ServiceLoader.load(classOf[ProfilerPlugin]).iterator.asScala.toList

  @annotation.nowarn("cat=deprecation")
  private[profile] def snapThread(idleTimeNanos: Long): ProfileSnap = {
    val current = Thread.currentThread()
    val allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId)
    ProfileSnap(
      threadId = current.getId,
      threadName = current.getName,
      snapTimeNanos = System.nanoTime(),
      idleTimeNanos = idleTimeNanos,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = allocatedBytes,
      heapBytes = readHeapUsage(),
      totalClassesLoaded = classLoaderMx.getTotalLoadedClassCount,
      totalJITCompilationTime = compileMx.getTotalCompilationTime
    )
  }
  private def readHeapUsage() = RealProfiler.memoryMx.getHeapMemoryUsage.getUsed
}

private [profile] class RealProfiler(reporter : ProfileReporter, val settings: Settings, costs: DefCosts = null) extends Profiler with NotificationListener {
  private val mainThread = Thread.currentThread()
  val id = RealProfiler.idGen.incrementAndGet()
  object Category {
    final val Run = "run"
    final val Phase = "phase"
    final val File = "file"
    final val TypeCheck = "typecheck"
    final val Implicit = "implicit"
    final val Macro = "macro"
    final val Completion = "completion"
  }

  private val chromeTrace = {
    if (settings.YprofileTrace.isSetByUser)
      new ChromeTrace(Paths.get(settings.YprofileTrace.value))
    else null
  }
  if (chromeTrace != null)
    chromeTrace.traceDurationEventStart(Category.Run, "scalac-" + id)

  def completeBackground(threadRange: ProfileRange): Unit = {
    reporter.reportBackground(this, threadRange)
  }

  def outDir = settings.outputDirs.getSingleOutput.map(_.path).getOrElse(settings.outputDirs.outputs.head._2.path)

  RealProfiler.gcMx foreach {
    case emitter: NotificationEmitter => emitter.addNotificationListener(this, null, null)
    case gc => println(s"Cant connect gcListener to ${gc.getClass}")
  }

  val active = RealProfiler.allPlugins map (_.generate(this, settings))

  private def doGC(): Unit = {
    System.gc()
    System.runFinalization(): @nowarn("cat=deprecation") // since Java 18
  }

  reporter.header(this)

  override def finished(): Unit = {
    if (costs != null) costs.finished()
    active foreach {_.finished()}
    //we may miss a GC event if gc is occurring as we call this
    RealProfiler.gcMx foreach {
      case emitter: NotificationEmitter => emitter.removeNotificationListener(this)
      case _ =>
    }
    reporter.close(this)
    if (chromeTrace != null) {
      for (gcEvent <- gcEvents) {
        val durationNanos = TimeUnit.MILLISECONDS.toNanos(gcEvent.durationMillis)
        val startNanos = gcEvent.endNanos - durationNanos
        chromeTrace.traceDurationEvent(gcEvent.name, startNanos, durationNanos, GcThreadId)
      }
      chromeTrace.traceDurationEventEnd(Category.Run, "scalac-" + id)
      chromeTrace.close()
    }
  }

  private val gcEvents = ArrayBuffer[GcEventData]()
  private val GcThreadId = "GC"

  override def handleNotification(notification: Notification, handback: scala.Any): Unit = {
    import java.lang.{Long => jLong}
    import java.lang.{Integer => jInt}
    val reportNs = System.nanoTime()
    val data = notification.getUserData
    //val seq = notification.getSequenceNumber
    //val message = notification.getMessage
    val tpe = notification.getType
    //val time= notification.getTimeStamp
    data match {
      case cd: CompositeData if tpe == "com.sun.management.gc.notification" =>
        val name = cd.get("gcName").toString
        val action = cd.get("gcAction").toString
        val cause = cd.get("gcCause").toString
        val info = cd.get("gcInfo").asInstanceOf[CompositeData]
        val duration = info.get("duration").asInstanceOf[jLong].longValue()
        val startTime = info.get("startTime").asInstanceOf[jLong].longValue()
        val endTime = info.get("endTime").asInstanceOf[jLong].longValue()
        val threads = info.get("GcThreadCount").asInstanceOf[jInt].longValue()
        val gcEvent = GcEventData("", reportNs, startTime, endTime, duration, name, action, cause, threads)
        synchronized {
          gcEvents += gcEvent
        }
        reporter.reportGc(gcEvent)
      case x => throw new MatchError(x)
    }
  }

  override def beforePhase(phase: Phase): ProfileSnap = {
    assert(mainThread eq Thread.currentThread())
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.Phase, phase.name)
    if (settings.YprofileRunGcBetweenPhases.containsPhase(phase))
      doGC()
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    active foreach {_.beforePhase(phase)}
    if (costs != null) costs.enter(CostKind.Phase, phase, phase)
    RealProfiler.snapThread(0)
  }

  override def afterPhase(phase: Phase, snapBefore: ProfileSnap): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = RealProfiler.snapThread(0)
    if (costs != null) costs.exit(CostKind.Phase, phase)
    active foreach {_.afterPhase(phase)}
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (settings.YprofileRunGcBetweenPhases.containsPhase(phase)) {
      doGC()
      initialSnap.updateHeap(RealProfiler.readHeapUsage())
    } else initialSnap
    if (chromeTrace != null) chromeTrace.traceDurationEventEnd(Category.Phase, phase.name)

    reporter.reportForeground(this, ProfileRange(snapBefore, finalSnap, phase, "", 0, Thread.currentThread))
  }

  override def beforeUnit(phase: Phase, file: AbstractFile): Unit = {
    assert(mainThread eq Thread.currentThread())
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.File, file.name)
    if (costs != null) costs.enter(CostKind.Unit, file, phase)
  }

  private var nextAfterUnitSnap: Long = System.nanoTime()

  override def afterUnit(phase: Phase, file: AbstractFile): Unit = {
    assert(mainThread eq Thread.currentThread())
    if (costs != null) costs.exit(CostKind.Unit, file)
    if (chromeTrace != null) {
      val now = System.nanoTime()
      chromeTrace.traceDurationEventEnd(Category.File, file.name)
      if (now > nextAfterUnitSnap) {
        val initialSnap = RealProfiler.snapThread(0)
        chromeTrace.traceCounterEvent("allocBytes", "allocBytes", initialSnap.allocatedBytes, processWide = false)
        chromeTrace.traceCounterEvent("heapBytes", "heapBytes", initialSnap.heapBytes, processWide = true)
        chromeTrace.traceCounterEvent("classesLoaded", "classesLoaded", initialSnap.totalClassesLoaded, processWide = true)
        chromeTrace.traceCounterEvent("jitCompilationTime", "jitCompilationTime", initialSnap.totalJITCompilationTime, processWide = true)
        chromeTrace.traceCounterEvent("userTime", "userTime", initialSnap.userTimeNanos, processWide = false)
        chromeTrace.traceCounterEvent("cpuTime", "cpuTime", initialSnap.cpuTimeNanos, processWide = false)
        chromeTrace.traceCounterEvent("idleTime", "idleTime", initialSnap.idleTimeNanos, processWide = false)
        nextAfterUnitSnap = System.nanoTime() + 10 * 1000 * 1000
      }
    }
  }

  override def beforeTypedImplDef(sym: Global#Symbol): Unit = {
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.TypeCheck, sym.rawname.toString)
    if (costs != null) costs.enter(CostKind.TypedDef, sym, NoPhase)
  }
  override def afterTypedImplDef(sym: Global#Symbol): Unit = {
    if (costs != null) costs.exit(CostKind.TypedDef, sym)
    if (chromeTrace != null) chromeTrace.traceDurationEventEnd(Category.TypeCheck, sym.rawname.toString)
  }

  override def beforeImplicitSearch(pt: Global#Type): Unit = {
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.Implicit, "?[" + pt.typeSymbol.rawname + "]", colour = "yellow")
    if (costs != null) costs.enter(CostKind.Implicit, null, NoPhase)
  }

  override def afterImplicitSearch(pt: Global#Type): Unit = {
    if (costs != null) costs.exit(CostKind.Implicit, null)
    if (chromeTrace != null) chromeTrace.traceDurationEventEnd(Category.Implicit, "?[" + pt.typeSymbol.rawname + "]", colour = "yellow")
  }

  override def beforeMacroExpansion(macroSym: Global#Symbol): Unit = {
    if (chromeTrace != null) chromeTrace.traceDurationEventStart(Category.Macro, "«" + macroSym.rawname + "»", colour = "olive")
    if (costs != null) costs.enter(CostKind.Macro, null, NoPhase)
  }

  override def afterMacroExpansion(macroSym: Global#Symbol): Unit = {
    if (costs != null) costs.exit(CostKind.Macro, null)
    if (chromeTrace != null) chromeTrace.traceDurationEventEnd(Category.Macro, "«" + macroSym.rawname + "»", colour = "olive")
  }

  override def beforeCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = {
    if (chromeTrace != null) {
      chromeTrace.traceDurationEventStart(Category.Completion, "↯", colour = "thread_state_sleeping")
      chromeTrace.traceDurationEventStart(Category.File, associatedFile.name)
      chromeTrace.traceDurationEventStart(Category.Completion, completionName(root, associatedFile))
    }
    if (costs != null) costs.enter(CostKind.Load, root, NoPhase)
  }

  override def afterCompletion(root: Global#Symbol, associatedFile: AbstractFile): Unit = {
    if (costs != null) costs.exit(CostKind.Load, root)
    if (chromeTrace != null) {
      chromeTrace.traceDurationEventEnd(Category.Completion, completionName(root, associatedFile))
      chromeTrace.traceDurationEventEnd(Category.File, associatedFile.name)
      chromeTrace.traceDurationEventEnd(Category.Completion, "↯", colour = "thread_state_sleeping")
    }
  }

  override def beforeNamerCompletion(sym: Global#Symbol, tree: Global#Tree): Unit =
    if (costs != null) {
      costs.definedBy(sym, tree)
      costs.enter(CostKind.Completion, sym, NoPhase)
    }
  override def afterNamerCompletion(sym: Global#Symbol): Unit =
    if (costs != null) costs.exit(CostKind.Completion, sym)

  private def completionName(root: Global#Symbol, @unused associatedFile: AbstractFile): String =
    if (root.hasPackageFlag || root.isTopLevel) root.javaBinaryNameString
    else {
      val enclosing = root.enclosingTopLevelClass
      enclosing.javaBinaryNameString + "::" + root.rawname.toString
    }
}

object EventType extends Enumeration {
  type value = Value
  //main thread with other tasks
  val MAIN = Value("main")
  //other task ( background thread)
  val BACKGROUND = Value("background")
  //total for compile
  val GC = Value("GC")
}

sealed trait ProfileReporter {
  def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit
  def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit

  def reportGc(data: GcEventData): Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  private val outWriter = new PrintWriter(Console.out)
  private val delegate = new StreamProfileReporter(new PrintWriter(Console.out))
  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit = delegate.reportBackground(profiler, threadRange)
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit = delegate.reportForeground(profiler, threadRange)
  override def close(profiler: RealProfiler): Unit = outWriter.flush()

  override def header(profiler: RealProfiler): Unit = delegate.header(profiler)
  override def reportGc(data: GcEventData): Unit = delegate.reportGc(data)
}

object NoOpProfileReporter extends ProfileReporter {
  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit = ()
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit = ()
  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit = ()
  override def reportGc(data: GcEventData): Unit = ()
}

class StreamProfileReporter(out:PrintWriter) extends ProfileReporter {
  @nowarn("cat=lint-inaccessible")
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, version, 2, output, ${profiler.outDir}")
    out.println(s"header(main/background),startNs,endNs,runId,phaseId,phaseName,purpose,task-count,threadId,threadName,runNs,idleNs,cpuTimeNs,userTimeNs,allocatedByte,heapSize")
    out.println(s"header(GC),startNs,endNs,startMs,endMs,name,action,cause,threads")
  }

  @nowarn("cat=lint-inaccessible")
  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  }
  @nowarn("cat=lint-inaccessible")
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    reportCommon(EventType.MAIN, profiler, threadRange)
  }
  @annotation.nowarn("cat=deprecation")
  private def reportCommon(tpe:EventType.value, profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    out.println(s"$tpe,${threadRange.start.snapTimeNanos},${threadRange.end.snapTimeNanos},${profiler.id},${threadRange.phase.id},${threadRange.phase.name},${threadRange.purpose},${threadRange.taskCount},${threadRange.thread.getId},${threadRange.thread.getName},${threadRange.runNs},${threadRange.idleNs},${threadRange.cpuNs},${threadRange.userNs},${threadRange.allocatedBytes},${threadRange.end.heapBytes} ")
  }

  override def reportGc(data: GcEventData): Unit = {
    val duration = TimeUnit.MILLISECONDS.toNanos(data.gcEndMillis - data.gcStartMillis + 1)
    val start = data.reportTimeNs - duration
    out.println(s"${EventType.GC},$start,${data.reportTimeNs},${data.gcStartMillis}, ${data.gcEndMillis},${data.name},${data.action},${data.cause},${data.threads}")
  }

  @nowarn("cat=lint-inaccessible")
  override def close(profiler: RealProfiler): Unit = {
    out.flush()
    out.close()
  }
}
