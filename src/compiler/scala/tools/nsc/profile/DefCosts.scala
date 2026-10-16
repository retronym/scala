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

import java.io.IOException
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.IdentityHashMap

import scala.annotation.nowarn
import scala.reflect.internal.{NoPhase, Phase}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Global

/** What a frame on the cost stack measures. */
sealed abstract class CostKind(val label: String, val perPhase: Boolean, val ordinal: Int)
object CostKind {
  /** A phase, outside its units */
  case object Phase extends CostKind("phase", perPhase = true, 0)
  /** A compilation unit in one phase, outside the frames below */
  case object Unit extends CostKind("unit", perPhase = true, 1)
  /** Typing a definition in the typer (`typedClassDef`, `typedModuleDef`, `typedValDef`, `typedDefDef`) */
  case object TypedDef extends CostKind("def", perPhase = false, 2)
  /** Completing a symbol from source (a Namer `TypeCompleterBase`): its signature, and its body when its type is inferred */
  case object Completion extends CostKind("complete", perPhase = false, 3)
  /** Completing a symbol from a classfile, or a package (`SymbolLoader`) */
  case object Load extends CostKind("load", perPhase = false, 4)
  /** An implicit search, charged to the innermost definition (or unit) */
  case object Implicit extends CostKind("implicit", perPhase = true, 5)
  /** A macro expansion, charged to the innermost definition (or unit) */
  case object Macro extends CostKind("macro", perPhase = true, 6)
  /** Typing a definition after the typer (erasure's retyping). Other work after the typer is charged to its unit. */
  case object Transform extends CostKind("transform", perPhase = true, 7)

  val values: Array[CostKind] = Array(Phase, Unit, TypedDef, Completion, Load, Implicit, Macro, Transform)
}

/** The cost of one kind of work for one key (a symbol, a source file or a phase) in one phase (`NoPhase` for the
 *  kinds that are not per phase), summed over its frames. Only with `-Yprofile-defs`. */
final class CostAcc(val kind: CostKind, val key: AnyRef, val phase: Phase) {
  var calls: Int = 0
  var selfNs: Long = 0L
  var selfAlloc: Long = 0L
  var totalNs: Long = 0L
  var maxSelfNs: Long = 0L
}

/** A region on the cost stack: from entering a hooked operation to leaving it. Frames are reused once left, so a
 *  listener must not keep one after `exit`. */
final class CostFrame private[profile] () {
  private[this] var myKind: CostKind = CostKind.Phase
  private[this] var myKey: AnyRef = null
  private[this] var myPhase: Phase = NoPhase
  private[this] var myParent: CostFrame = null
  // with -Yprofile-defs only: the accumulator, and the clocks
  private[profile] var acc: CostAcc = null
  private[profile] var startNs: Long = 0L
  private[profile] var startAlloc: Long = 0L
  private[profile] var childNs: Long = 0L
  private[profile] var childAlloc: Long = 0L
  /** For a listener's own use, e.g. when the frame's span started */
  var listenerData: Long = 0L

  private[profile] def init(kind: CostKind, key: AnyRef, phase: Phase, parent: CostFrame): Unit = {
    myKind = kind; myKey = key; myPhase = phase; myParent = parent
    acc = null; childNs = 0L; childAlloc = 0L; listenerData = 0L
  }

  def kind: CostKind = myKind
  /** A `Global#Symbol`, the `AbstractFile` of a unit for `CostKind.Unit`, a `Phase` for `CostKind.Phase`, or null
   *  for an implicit search or macro expansion outside any frame */
  def key: AnyRef = myKey
  /** The phase, for the kinds that are per phase; else `NoPhase` */
  def phase: Phase = myPhase
  def parent: CostFrame = myParent
  /** A short name: the phase's name, the unit's source path, the symbol's (unqualified) name, or "implicit" / "macro".
   *  The frames enclosing it say what it is part of. A symbol's name is a new string. */
  def label: String = DefCosts.labelOf(myKind, myKey)
}

/** Told about each frame as it is entered and left, on the compiling thread: e.g. to record async-profiler spans
 *  nested as the frames are. Named by `-Yprofile-defs-listener`; needs a public no-argument constructor.
 */
abstract class CostListener {
  /** `frame` was entered, inside its parent */
  def enter(frame: CostFrame): Unit
  /** `frame` was left; it is no longer on the stack */
  def exit(frame: CostFrame): Unit
  def finished(): Unit = ()
}

/** Exclusive cost per definition. A stack of frames is kept in step with the profiler's hooks: a phase, a compilation
 *  unit in a phase, typing a definition (in the typer, or retyping it after it), completing a symbol from source or
 *  loading one from a classfile, an implicit search and a macro expansion. A frame's self time is its time minus that
 *  of the frames nested in it, so the completion of `b` that happens while typing `a` (to infer `b`'s result type,
 *  say) is charged to `b`, not to `a`.
 *
 *  Only class members and top-level definitions get frames of their own (including members of local and anonymous
 *  classes); local definitions are part of the definition they are in. Implicit searches and macro expansions are
 *  charged to the innermost frame's key. Only the thread that created the tracker is measured.
 *
 *  With a listener alone, a frame is its kind, key, phase and parent; clocks and the accumulators are only kept for
 *  the table (`path` non-empty).
 */
final class DefCosts(val global: Global, listener: CostListener, path: String) {
  import DefCosts._

  private[this] val thread = Thread.currentThread()
  @nowarn("cat=deprecation") // Thread.getId, since JDK 19
  private[this] val threadId = thread.getId
  private[this] val table = path.nonEmpty
  // per kind, per phase (NoPhase for kinds not per phase), per key; only for the table
  private[this] val accs =
    if (table) Array.fill(CostKind.values.length)(new IdentityHashMap[Phase, IdentityHashMap[AnyRef, CostAcc]]())
    else null
  // a definition's size in characters, from its tree's range position; only for the table
  private[this] val sizes = if (table) new IdentityHashMap[AnyRef, Integer]() else null
  private[this] var top: CostFrame = null
  // frames by depth, reused
  private[this] var pool = new Array[CostFrame](64)
  private[this] var depth = 0
  private[this] var typerPhaseId = -1

  private def allocated(): Long = threadMx.getThreadAllocatedBytes(threadId)

  private def pastTyper: Boolean = {
    if (typerPhaseId < 0) typerPhaseId = global.currentRun.typerPhase.id
    global.globalPhase.id > typerPhaseId
  }

  private def kindOf(kind: CostKind): CostKind =
    if ((kind eq CostKind.TypedDef) && pastTyper) CostKind.Transform else kind

  /** Enters a frame for `kind` and `key`, unless this operation gets no frame of its own. `phase` is the phase of a
   *  `Phase` or `Unit` frame; other frames that are per phase take the innermost unit frame's. For implicit searches
   *  and macro expansions the key is the innermost frame's. */
  def enter(kind0: CostKind, key: AnyRef, phase: Phase): Unit = {
    if (Thread.currentThread() ne thread) return
    val kind = kindOf(kind0)
    val k: AnyRef = kind match {
      case CostKind.Implicit | CostKind.Macro => if (top == null) null else top.key
      case CostKind.Unit | CostKind.Phase => key
      case _ => if (isTracked(key)) key else return
    }
    val ph =
      if (!kind.perPhase) NoPhase
      else if ((kind eq CostKind.Unit) || (kind eq CostKind.Phase)) phase
      else unitPhase(global.globalPhase)
    if (depth == pool.length) pool = java.util.Arrays.copyOf(pool, depth * 2)
    var frame = pool(depth)
    if (frame == null) {
      frame = new CostFrame()
      pool(depth) = frame
    }
    frame.init(kind, k, ph, top)
    if (table) {
      frame.acc = accFor(kind, k, ph)
      frame.startNs = System.nanoTime()
      frame.startAlloc = allocated()
    }
    depth += 1
    top = frame
    if (listener != null) {
      var entered = false
      try {
        listener.enter(frame)
        entered = true
      } finally if (!entered) pop(frame) // the caller's finally never sees this frame
    }
  }

  /** Notes the size of `sym`'s definition, `tree`, for the table */
  def definedBy(sym: Global#Symbol, tree: Global#Tree): Unit =
    if (sizes != null) {
      val pos = tree.pos
      if (pos.isRange) {
        val size = Integer.valueOf(pos.end - pos.start)
        sizes.put(sym, size)
        if (sym.isModuleNotMethod) sizes.put(sym.moduleClass, size) // completed with the module, not on its own
      }
    }

  /** Leaves the frame that `enter(kind, key, _)` entered, if it entered one. A frame left inside it (when a hook's
   *  `after` is not in a `finally`) is left first. */
  def exit(kind0: CostKind, key: AnyRef): Unit = {
    if (Thread.currentThread() ne thread) return
    val kind = kindOf(kind0)
    val byKind = (kind eq CostKind.Implicit) || (kind eq CostKind.Macro)
    var f = top
    while (f != null && !((f.kind eq kind) && (byKind || (f.key eq key)))) f = f.parent
    if (f != null) { // else `enter` made no frame: an untracked symbol
      while (top ne f) exitTop()
      exitTop()
    }
  }

  private def accFor(kind: CostKind, k: AnyRef, ph: Phase): CostAcc = {
    val byPhase = accs(kind.ordinal)
    var byKey = byPhase.get(ph)
    if (byKey == null) {
      byKey = new IdentityHashMap[AnyRef, CostAcc]()
      byPhase.put(ph, byKey)
    }
    val known = byKey.get(k)
    if (known != null) known
    else {
      val a = new CostAcc(kind, k, ph)
      byKey.put(k, a)
      a
    }
  }

  private def unitPhase(default: Phase): Phase = {
    var f = top
    while (f != null && (f.kind ne CostKind.Unit)) f = f.parent
    if (f == null) default else f.phase
  }

  private def pop(frame: CostFrame): Unit = {
    top = frame.parent
    depth -= 1
  }

  private def exitTop(): Unit = {
    val frame = top
    pop(frame)
    val acc = frame.acc
    if (acc != null) {
      val total = System.nanoTime() - frame.startNs
      val alloc = allocated() - frame.startAlloc
      val self = total - frame.childNs
      acc.calls += 1
      acc.selfNs += self
      acc.selfAlloc += alloc - frame.childAlloc
      acc.totalNs += total
      if (self > acc.maxSelfNs) acc.maxSelfNs = self
      val parent = frame.parent
      if (parent != null) {
        parent.childNs += total
        parent.childAlloc += alloc
      }
    }
    if (listener != null) listener.exit(frame)
  }

  /** One row per kind, phase and key, tab-separated, sorted by kind, phase and name: kind, phase (empty for kinds not
   *  per phase), name (a symbol's full name, with `$` for a module class, a unit's source path, a phase's name), file,
   *  line, chars (the size of the definition, from the range position of the tree its Namer completer has), calls, self and total time in µs, the longest single
   *  self time in µs, and self allocation in bytes. */
  def write(path: String): Unit = {
    import scala.jdk.CollectionConverters._
    val rows = for {
      byPhase <- accs.toList
      byKey <- byPhase.values.asScala.toList
      acc <- byKey.values.asScala.toList
    } yield (acc.kind.ordinal, phaseName(acc.phase), nameOf(acc.key), acc)
    val sb = new java.lang.StringBuilder("kind\tphase\tsymbol\tfile\tline\tchars\tcalls\tself_us\ttotal_us\tmax_self_us\tself_alloc\n")
    for ((_, phase, name, acc) <- rows.sortBy(r => (r._1, r._2, r._3))) {
      val (file, line, chars) = where(acc.key)
      sb.append(acc.kind.label).append('\t').append(phase).append('\t').append(name).append('\t')
        .append(file).append('\t').append(line).append('\t').append(chars).append('\t').append(acc.calls).append('\t')
        .append(acc.selfNs / 1000).append('\t').append(acc.totalNs / 1000).append('\t').append(acc.maxSelfNs / 1000)
        .append('\t').append(acc.selfAlloc).append('\n')
    }
    Files.write(Paths.get(path), sb.toString.getBytes(StandardCharsets.UTF_8))
  }

  def finished(): Unit = {
    if (listener != null) listener.finished()
    if (table) {
      try write(path)
      catch { case ex: IOException => global.globalError(s"-Yprofile-defs: cannot write $path: $ex") }
    }
  }

  private def isTracked(key: AnyRef): Boolean = key match {
    // not `exists`, which completes the symbol
    case sym: Global#Symbol => (sym ne global.NoSymbol) && sym.owner.isClass
    case _ => false
  }

  private def nameOf(key: AnyRef): String = key match {
    case null => "<outside a definition>"
    case sym: Global#Symbol => if (sym.isModuleClass) sym.fullName + "$" else sym.fullName
    case other => keyName(other)
  }

  private def where(key: AnyRef): (String, String, String) = key match {
    case file: AbstractFile => (file.path, "", "")
    case sym: Global#Symbol =>
      val pos = sym.pos
      if (!pos.isDefined) ("", "", "")
      else {
        val size = sizes.get(sym)
        (pos.source.path, pos.line.toString, if (size == null) "" else size.toString)
      }
    case _ => ("", "", "")
  }
}

object DefCosts {
  private val threadMx = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]

  private def phaseName(phase: Phase): String = if (phase eq NoPhase) "" else phase.name

  /** A frame's short label */
  private[profile] def labelOf(kind: CostKind, key: AnyRef): String = kind match {
    case CostKind.Implicit | CostKind.Macro => kind.label
    case _ => key match {
      case null => "<outside a definition>"
      case sym: Global#Symbol => sym.name.decode
      case other => keyName(other)
    }
  }

  private def keyName(key: AnyRef): String = key match {
    case phase: Phase => phase.name
    case file: AbstractFile => file.path
    case other => String.valueOf(other)
  }

  /** A tracker for this run, or null if neither `-Yprofile-defs` nor `-Yprofile-defs-listener` is set (or the listener
   *  cannot be created, which is reported). */
  def apply(global: Global): DefCosts = {
    val settings = global.settings
    val path = settings.YprofileDefs.value
    val listenerClass = settings.YprofileDefsListener.value
    val listener =
      if (listenerClass.isEmpty) null
      else
        try {
          val loader = Option(Thread.currentThread().getContextClassLoader).getOrElse(getClass.getClassLoader)
          Class.forName(listenerClass, true, loader).getDeclaredConstructor().newInstance().asInstanceOf[CostListener]
        } catch {
          case ex @ (_: ReflectiveOperationException | _: ClassCastException) =>
            global.globalError(s"-Yprofile-defs-listener: cannot create $listenerClass (a CostListener with a public no-argument constructor): $ex")
            null
        }
    if (path.isEmpty && listener == null) null
    else new DefCosts(global, listener, path)
  }
}
