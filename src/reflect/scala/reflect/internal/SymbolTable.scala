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

package scala
package reflect
package internal

import java.net.URLClassLoader

import scala.annotation.{elidable, nowarn, tailrec}
import scala.collection.mutable
import util._
import java.util.concurrent.TimeUnit

import scala.reflect.internal.settings.MutableSettings
import scala.reflect.internal.{TreeGen => InternalTreeGen}
import scala.reflect.io.AbstractFile

abstract class SymbolTable extends macros.Universe
                              with Collections
                              with Names
                              with Symbols
                              with Types
                              with Variances
                              with Kinds
                              with ExistentialsAndSkolems
                              with FlagSets
                              with Scopes
                              with Mirrors
                              with Definitions
                              with Constants
                              with BaseTypeSeqs
                              with InfoTransformers
                              with transform.Transforms
                              with StdNames
                              with AnnotationInfos
                              with AnnotationCheckers
                              with Trees
                              with Printers
                              with Positions
                              with TypeDebugging
                              with Importers
                              with CapturedVariables
                              with StdAttachments
                              with StdCreators
                              with ReificationSupport
                              with PrivateWithin
                              with pickling.Translations
                              with FreshNames
                              with Internals
                              with Reporting
{

  val gen = new InternalTreeGen { val global: SymbolTable.this.type = SymbolTable.this }

  trait ReflectStats extends BaseTypeSeqsStats
                        with TypesStats
                        with SymbolTableStats
                        with TreesStats
                        with SymbolsStats
                        with ScopeStats { self: Statistics => }

  /** Some statistics (normally disabled) set with -Ystatistics */
  val statistics: Statistics with ReflectStats

  def log(msg: => AnyRef): Unit

  protected def elapsedMessage(msg: String, startNs: Long) =
    msg + " in " + (TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNs)) + "ms"

  def informProgress(msg: String)            = if (settings.verbose.value) inform("[" + msg + "]")
  def informTime(msg: String, startNs: Long) = if (settings.verbose.value) informProgress(elapsedMessage(msg, startNs))
  @inline final def informingProgress[T](msg: => String)(fn: => T) : T = {
    val verbose: Boolean = settings.verbose.value
    val start = if (verbose) System.nanoTime() else 0L
    try fn finally if (verbose) informTime(msg, start)
  }

  def shouldLogAtThisPhase = false
  def isPastTyper = false

  @inline final def isDeveloper: Boolean = settings.isDebug || settings.isDeveloper

  def picklerPhase: Phase
  def erasurePhase: Phase

  def settings: MutableSettings

  def isSymbolLockTracingEnabled: Boolean = isDeveloper

  /** Override with final implementation for inlining. */
  def debuglog(msg:  => String): Unit = if (settings.isDebug) log(msg)

  /** dev-warns if dev-warning is enabled and `cond` is true; no-op otherwise */
  @inline final def devWarningIf(cond: => Boolean)(msg: => String): Unit =
    if (isDeveloper && cond) devWarning(msg)
  def devWarning(msg: => String): Unit = if (isDeveloper) Console.err.println(msg)
  def throwableAsString(t: Throwable): String = "" + t
  def throwableAsString(t: Throwable, maxFrames: Int): String = t.getStackTrace take maxFrames mkString "\n  at "

  @inline final def devWarningDumpStack(msg: => String, maxFrames: Int): Unit =
    devWarning(msg + "\n" + throwableAsString(new Throwable, maxFrames))

  /** Prints a stack trace if -Ydebug or equivalent was given, otherwise does nothing. */
  def debugStack(t: Throwable): Unit  = devWarning(throwableAsString(t))

  private[scala] def printCaller[T](msg: String)(result: T) = {
    Console.err.println("%s: %s\nCalled from: %s".format(msg, result,
      (new Throwable).getStackTrace.drop(2).take(50).mkString("\n")))

    result
  }

  private[scala] def printResult[T](msg: String)(result: T) = {
    Console.err.println(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def logResult[T](msg: => String)(result: T): T = {
    log(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def debuglogResult[T](msg: => String)(result: T): T = {
    debuglog(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def devWarningResult[T](msg: => String)(result: T): T = {
    devWarning(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def logResultIf[T](msg: => String, cond: T => Boolean)(result: T): T = {
    if (cond(result))
      log(msg + ": " + result)

    result
  }
  @inline
  final private[scala] def debuglogResultIf[T](msg: => String, cond: T => Boolean)(result: T): T = {
    if (cond(result))
      debuglog(msg + ": " + result)

    result
  }

  @inline
  final def assert(assertion: Boolean, message: => Any): Unit =
    if (!assertion) throwAssertionError(message)

  @deprecated("consider supplying an explanatory message", since = "2.12.5")
  final def assert(assertion: Boolean): Unit = assert(assertion, "")

  @inline
  final def require(requirement: Boolean, message: => Any): Unit =
    if (!requirement) throwRequirementError(message)

  @deprecated("consider supplying an explanatory message", since = "2.12.5")
  final def require(requirement: Boolean): Unit = require(requirement, "")

  // extracted from `assert`/`require` to make them as small (and inlineable) as possible
  private[internal] def throwAssertionError(msg: Any): Nothing =
    throw new java.lang.AssertionError(s"assertion failed: ${supplementErrorMessage(String valueOf msg)}")
  private[internal] def throwRequirementError(msg: Any): Nothing =
    throw new java.lang.IllegalArgumentException(s"requirement failed: ${supplementErrorMessage(String valueOf msg)}")

  @inline final def findSymbol(xs: IterableOnce[Symbol])(p: Symbol => Boolean): Symbol = {
    xs.iterator find p getOrElse NoSymbol
  }

  // For too long have we suffered in order to sort NAMES.
  // I'm pretty sure there's a reasonable default for that.
  // Notice challenge created by Ordering's invariance.
  implicit def lowPriorityNameOrdering[T <: Names#Name]: Ordering[T] =
    SimpleNameOrdering.asInstanceOf[Ordering[T]]

  private object SimpleNameOrdering extends Ordering[Names#Name] {
    def compare(n1: Names#Name, n2: Names#Name) = (
      if (n1 eq n2) 0
      else n1.toString compareTo n2.toString
    )
  }

  /** Dump each symbol to stdout after shutdown.
   */
  final val traceSymbolActivity = System.getProperty("scalac.debug.syms") != null
  object traceSymbols extends {
    val global: SymbolTable.this.type = SymbolTable.this
  } with util.TraceSymbolActivity

  val treeInfo: TreeInfo { val global: SymbolTable.this.type }

  /** Check that the executing thread is the compiler thread. No-op here,
   *  overridden in interactive.Global. */
  @elidable(elidable.WARNING)
  def assertCorrectThread(): Unit = {}

  /** A last effort if symbol in a select <owner>.<name> is not found.
   *  This is overridden by the reflection compiler to make up a package
   *  when it makes sense (i.e. <owner> is a package and <name> is a term name).
   */
  def missingHook(owner: Symbol, name: Name): Symbol = NoSymbol

  /** Returns the mirror that loaded given symbol */
  def mirrorThatLoaded(sym: Symbol): Mirror

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   */
  type Period = Int
  final val NoPeriod = 0

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0

  private[this] val phStack: collection.mutable.Stack[Phase] = new collection.mutable.Stack()
  private[this] var ph: Phase = NoPhase
  private[this] var per = NoPeriod

  /** Whether the phase and period are kept per thread, in `Scratch`: in a compiler used from several threads at
   *  once (`-Yparallel-typer`), each thread is at a phase of its own (`enteringPhase` on one thread must not move
   *  another). Not in runtime reflection, which is always at one phase. Read while the universe is constructed:
   *  until this field is initialized, the phase is the shared one, which is only set later.
   */
  private[this] val phasePerThread: Boolean = isCompilerUniverse && isConcurrentUniverse
  /** Whether this is a compiler used from several threads at once (`-Yparallel-typer`). */
  final def isConcurrentCompiler: Boolean = phasePerThread

  final def atPhaseStack: List[Phase] = phStack.toList
  final def phase: Phase = {
    if (phasePerThread) scratch.ph else ph
  }

  def atPhaseStackMessage = atPhaseStack match {
    case Nil    => ""
    case ps     => ps.reverseIterator.map("->" + _).mkString("(", " ", ")")
  }

  final def phase_=(p: Phase): Unit = {
    if (phasePerThread) {
      val s = scratch
      s.ph = p
      s.per = period(currentRunId, p.id)
    } else {
      ph = p
      per = period(currentRunId, p.id)
    }
  }
  final def pushPhase(ph: Phase): Phase = {
    val current = phase
    phase = ph
    if (keepPhaseStack) {
      phStack.push(ph)
    }
    current
  }
  final def popPhase(ph: Phase): Unit = {
    if (keepPhaseStack) {
      phStack.pop()
    }
    phase = ph
  }
  var keepPhaseStack: Boolean = false

  /** The current compiler run identifier. */
  def currentRunId: RunId

  /** The run identifier of the given period. */
  final def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period. */
  final def phaseId(period: Period): Phase#Id = period & 0xFF

  /** The current period. */
  final def currentPeriod: Period = {
    //assert(per == (currentRunId << 8) + phase.id)
    if (phasePerThread) scratch.per else per
  }

  /** The phase associated with given period. */
  final def phaseOf(period: Period): Phase = phaseWithId(phaseId(period))

  final def period(rid: RunId, pid: Phase#Id): Period =
    (rid << 8) + pid

  /** Are we later than given phase in compilation? */
  final def isAtPhaseAfter(p: Phase) =
    p != NoPhase && phase.id > p.id

  /** Perform given operation at given phase. */
  @inline final def enteringPhase[T](ph: Phase)(op: => T): T = {
    if (ph eq phase) op // opt
    else {
      val saved = pushPhase(ph)
      try op
      finally popPhase(saved)
    }
  }

  final def findPhaseWithName(phaseName: String): Phase = {
    var ph = phase
    while (ph != NoPhase && ph.name != phaseName) {
      ph = ph.prev
    }
    if (ph eq NoPhase) phase else ph
  }
  final def enteringPhaseWithName[T](phaseName: String)(body: => T): T = {
    val phase = findPhaseWithName(phaseName)
    enteringPhase(phase)(body)
  }

  def slowButSafeEnteringPhase[T](ph: Phase)(op: => T): T = {
    if (isCompilerUniverse) enteringPhase(ph)(op)
    else op
  }

  @inline final def exitingPhase[T](ph: Phase)(op: => T): T = enteringPhase(ph.next)(op)
  @inline final def enteringPrevPhase[T](op: => T): T       = enteringPhase(phase.prev)(op)

  @inline final def enteringPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (isAtPhaseAfter(target)) enteringPhase(target)(op) else op

  def slowButSafeEnteringPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (isCompilerUniverse) enteringPhaseNotLaterThan(target)(op) else op

  final def isValid(period: Period): Boolean =
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) nextFrom(pid).pid >= phase.id
      else nextFrom(phase.id).pid >= pid
    }

  final def isValidForBaseClasses(period: Period): Boolean = {
    @tailrec
    def noChangeInBaseClasses(it: InfoTransformer, limit: Phase#Id): Boolean = (
      it.pid >= limit ||
      !it.changesBaseClasses && noChangeInBaseClasses(it.next, limit)
    )
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) noChangeInBaseClasses(nextFrom(pid), phase.id)
      else noChangeInBaseClasses(nextFrom(phase.id), pid)
    }
  }

  def openPackageModule(container: Symbol, dest: Symbol): Unit = {
    // unlink existing symbols in the package
    for (member <- container.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        // todo: handle overlapping definitions in some way: mark as errors
        // or treat as abstractions. For now the symbol in the package module takes precedence.
        for (existing <- dest.info.decl(member.name).alternatives)
          dest.info.decls.unlink(existing)
      }
    }
    // enter non-private decls the class
    for (member <- container.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        dest.info.decls.enter(member)
      }
    }
    // enter decls of parent classes
    for (p <- container.parentSymbolsIterator) {
      if (p != definitions.ObjectClass) {
        openPackageModule(p, dest)
      }
    }
  }

  /** Convert array parameters denoting a repeated parameter of a Java method
   *  to `JavaRepeatedParamClass` types.
   */
  def arrayToRepeated(tp: Type): Type = tp match {
    case MethodType(params, rtpe) =>
      val formals = tp.paramTypes
      assert(formals.last.typeSymbol == definitions.ArrayClass, formals)
      val method = params.last.owner
      val elemtp = formals.last.typeArgs.head match {
        case RefinedType(List(t1, t2), _) if (t1.typeSymbol.isAbstractType && t2.typeSymbol == definitions.ObjectClass) =>
          t1 // drop intersection with Object for abstract types in varargs. UnCurry can handle them.
        case t =>
          t
      }
      val newParams = method.newSyntheticValueParams(formals.init :+ definitions.javaRepeatedType(elemtp))
      MethodType(newParams, rtpe)
    case PolyType(tparams, rtpe) => PolyType(tparams, arrayToRepeated(rtpe))
    case x                       => throw new MatchError(x)
  }

  abstract class SymLoader extends LazyType {
    def fromSource = false
  }

  /** if there's a `package` member object in `pkgClass`, enter its members into it. */
  def openPackageModule(pkgClass: Symbol, force: Boolean = false): Unit = {
    val pkgModule  = pkgClass.packageObject
    def fromSource = pkgModule.rawInfo match {
      case ltp: SymLoader => ltp.fromSource
      case _ => false
    }
    if (pkgModule.isModule && !fromSource)
      openPackageModule(pkgModule, pkgClass)
  }

  object perRunCaches {
    import scala.collection.mutable.Clearable

    // Weak references so the garbage collector will take care of
    // letting us know when a cache is really out of commission.
    import java.lang.ref.WeakReference
    private[this] var caches = List[WeakReference[Clearable]]()
    private[this] var javaCaches = List[JavaClearable[_]]()

    def recordCache[T <: Clearable](cache: T): cache.type = synchronized {
      cache match {
        case jc: JavaClearable[_] =>
          javaCaches ::= jc
        case _ =>
          caches ::= new WeakReference(cache)
      }
      cache
    }

    /** Closes the provided classloader at the conclusion of this Run */
    final def recordClassloader(loader: ClassLoader): loader.type = {
      def attemptClose(loader: ClassLoader): Unit = {
        loader match {
          case u: URLClassLoader => debuglog("Closing classloader " + u); u.close()
          case _ =>
        }
      }
      caches ::= new WeakReference((new Clearable {
        def clear(): Unit = {
          attemptClose(loader)
        }
      }))
      loader
    }

    /**
     * Removes a cache from the per-run caches. This is useful for testing: it allows running the
     * compiler and then inspect the state of a cache.
     */
    def unrecordCache[T <: Clearable](cache: T): Unit = {
      cache match {
        case jc: JavaClearable[_] =>
          javaCaches = javaCaches.filterNot(cache == _)
        case _ =>
          caches = caches.filterNot(_.get eq cache)
      }
    }

    def clearAll(): Unit = {
      debuglog("Clearing " + (caches.size + javaCaches.size) + " caches.")
      caches foreach (ref => Option(ref.get).foreach(_.clear()))
      caches = caches.filterNot(_.get == null)

      javaCaches foreach (_.clear())
      javaCaches = javaCaches.filter(_.isValid)
    }

    def newWeakMap[K, V]()        = recordCache(mutable.WeakHashMap.empty[K, V])
    def newMap[K, V]()            = recordCache(mutable.HashMap.empty[K, V])
    def newSet[K]()               = recordCache(mutable.HashSet.empty[K])
    def newWeakSet[K <: AnyRef]() = recordCache(WeakHashSet.empty[K])

    @nowarn("cat=deprecation")
    def newAnyRefMap[K <: AnyRef, V]() = recordCache(mutable.AnyRefMap.empty[K, V])
    @nowarn("cat=deprecation")
    def newAnyRefMap[K <: AnyRef, V](default: K => V) = recordCache(mutable.AnyRefMap.withDefault[K, V](default))

    /**
      * Register a cache specified by a factory function and (optionally) a cleanup function.
      *
      * @return A function that will return cached value, or create a fresh value when a new run is started.
      */
    def newGeneric[T](f: => T, cleanup: T => Unit = (x: Any) => ()): () => T = {
      val NoCached: T = null.asInstanceOf[T]
      var cached: T = NoCached
      var cachedRunId = NoRunId
      val clearable = new Clearable with (() => T)  {
        def clear(): Unit = {
          if (cached != NoCached)
            cleanup(cached)
          cached = NoCached
        }
        def apply(): T = {
          if (currentRunId != cachedRunId || cached == NoCached) {
            cached = f
            cachedRunId = currentRunId
          }
          cached
        }
      }
      recordCache(clearable)
    }
  }

  /** The set of all installed infotransformers. */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }
  var nextFrom: Array[InfoTransformer] = null

  private final val MaxPhases = 256
  /** The phase which has given index as identifier. */
  final val phaseWithId: Array[Phase] = Array.fill(MaxPhases)(NoPhase)

  /** Is this symbol table a part of a compiler universe?
   */
  def isCompilerUniverse = false

  /** Is this universe used from several threads at once? Runtime reflection is.
   *
   *  The universe chooses, once, the implementations of the state that this decides: one `Scratch`
   *  or one per thread, plain or atomic symbol ids, plain or synchronized interning. The batch
   *  compiler gets the single-threaded ones and pays for the choice with a call through a field.
   *  Must be a constant: it is read while the universe is constructed.
   */
  def isConcurrentUniverse = false

  override protected def synchronizeNames: Boolean = isConcurrentUniverse

  /** In a compiler used from several threads (`isConcurrentCompiler`), the lock under which every completion runs;
   *  null otherwise. See `Symbol.info`.
   */
  final lazy val completionLock: util.CompletionLock = if (isConcurrentCompiler) new util.CompletionLock else null

  /** A value of each thread in a concurrent compiler, else one value: for the scratch state of the typer
   *  (caches and side channels kept in fields of the cake) that must be confined to a thread. Must be created after
   *  this universe is constructed. */
  final class PerThread[T <: AnyRef](init: () => T) {
    private[this] val single: T = if (isConcurrentCompiler) null.asInstanceOf[T] else init()
    private[this] val local: ThreadLocal[T] = if (isConcurrentCompiler) ThreadLocal.withInitial(() => init()) else null
    def get: T = if (local eq null) single else local.get
  }
  final def perThread[T <: AnyRef](init: => T): PerThread[T] = new PerThread(() => init)

  /** `body` under the completion lock, in a concurrent compiler. */
  @inline final def underCompletionLock[T](body: => T): T = {
    val lock = completionLock
    if (lock eq null) body
    else {
      lock.acquire()
      try body
      finally lock.release()
    }
  }

  /** The recursion guards of the type operations at one level of completion, with the undo log
   *  of type constraints. Each operation fetches its frame once and keeps it in a local.
   */
  final class Frame {
    private[this] var _next: Frame = null
    /** The frame for a completion started from this one. Kept for reuse once allocated. */
    def next: Frame = {
      if (_next eq null) _next = new Frame
      _next
    }
    /** Prevents recursion on `K`s, starting only once `threshold` calls are in progress: guarding
     *  every call would be too expensive, and this recursion is rare. On a cycle, the caller decides
     *  what the answer is. `enter` and `exit` keep hot paths free of closures.
     */
    final class RecursionGuard[K](threshold: Int) {
      private[this] var _depth: Int = 0
      def depth: Int = _depth
      private[this] var _pending: mutable.HashSet[K] = null
      def pending: mutable.HashSet[K] = {
        if (_pending eq null) _pending = new mutable.HashSet[K]
        _pending
      }
      /** False if `key` is already being computed. Otherwise the caller must call `exit`. */
      def enter(key: K): Boolean = {
        _depth += 1
        _depth < threshold || pending.add(key) || { _depth -= 1; false }
      }
      def exit(key: K): Unit = {
        if (_depth >= threshold) pending -= key
        _depth -= 1
      }
    }

    /** A `RecursionGuard` for subtype checks. A check must not allocate its key unless it is past
     *  the threshold, and a cycle is remembered for the rest of the outermost check.
     */
    final class SubTypeGuard(threshold: Int) {
      private[this] var _depth: Int = 0
      def depth: Int = _depth
      private[this] var _pending: mutable.HashSet[SubTypePair] = null
      def pending: mutable.HashSet[SubTypePair] = {
        if (_pending eq null) _pending = new mutable.HashSet[SubTypePair]
        _pending
      }
      private[this] var _knownFalse: java.util.IdentityHashMap[Type, List[Type]] = null
      def knownFalse: java.util.IdentityHashMap[Type, List[Type]] = {
        if (_knownFalse eq null) _knownFalse = new java.util.IdentityHashMap[Type, List[Type]]
        _knownFalse
      }
      private def isKnownFalse(tp1: Type, tp2: Type): Boolean =
        (_knownFalse ne null) && !_knownFalse.isEmpty && { val ts = _knownFalse.get(tp1); ts != null && ts.contains(tp2) }
      private def recordKnownFalse(tp1: Type, tp2: Type): Unit = { // see scala/bug#13119
        val ts = knownFalse.get(tp1)
        knownFalse.put(tp1, tp2 :: (if (ts != null) ts else Nil))
      }

      /** Enter a check with no key of its own, such as `isSameType`. */
      def enter(): Unit = _depth += 1
      def exit(): Unit = {
        _depth -= 1
        // Called on every top-level check, but the map is only populated for pathological recursive
        // subtype checks. Guard the `clear()`, as `IdentityHashMap.clear` zeroes the entire table even when empty.
        if (_depth == 0 && (_knownFalse ne null) && !_knownFalse.isEmpty) _knownFalse.clear()
        // undoLog.clear() // too eager, breaks #3281, #3866
      }

      /** False if `tp1 <:< tp2` is already being checked or is known to be false, which is the answer
       *  (see neg/t8146-no-finitary*). Otherwise the caller must call `exit`.
       */
      def enter(tp1: Type, tp2: Type): Boolean = {
        enter()
        val proceed =
          if (_depth >= threshold) pending.add(SubTypePair(tp1, tp2)) || { recordKnownFalse(tp1, tp2); false }
          else !isKnownFalse(tp1, tp2)
        if (!proceed) exit()
        proceed
      }
      def exit(tp1: Type, tp2: Type): Unit = {
        if (_depth >= threshold) pending -= SubTypePair(tp1, tp2)
        exit()
      }
    }

    val subTypes = new SubTypeGuard(TypeConstants.DefaultLogThreshhold)
    /** A log of type variable with their original constraints. Used in order
     *  to undo constraints in the case of isSubType/isSameType failure.
     */
    val undoLog = new UndoLog
    val baseTypes = new RecursionGuard[Type](TypeConstants.LogPendingBaseTypesThreshold)
    val volatiles = new RecursionGuard[Symbol](TypeConstants.LogVolatileThreshold)
    /** Memoizes `K`s and prevents recursion on them, for the duration of one operation. A key in
     *  progress is recorded with a private placeholder, which a real result can't be mistaken for.
     */
    final class MemoGuard[K] {
      private[this] val InProgress: Type = new Type {}
      val results = new mutable.HashMap[K, Type]
      /** The result for `key`, or `onCycle` if it is in progress, or `null` if neither. */
      def lookup(key: K, onCycle: Type): Type = results.getOrElse(key, null) match {
        case InProgress => onCycle
        case r          => r
      }
      def isInProgress(key: K): Boolean = results.get(key).contains(InProgress)
      def begin(key: K): Unit = results(key) = InProgress
      def end(key: K, result: Type): Unit = results(key) = result
    }

    val lubs = new MemoGuard[(Depth, List[Type])]
    val glbs = new MemoGuard[(Depth, List[Type])]
    /** Whether an outermost `lub` or `glb` is in progress in this frame. It owns the tables above. */
    var inLubOrGlb: Boolean = false
    /** How many `glb`s of refinements are adding members, see `GlbLubs.globalGlbLimit`. */
    var globalGlbDepth: Depth = Depth.Zero
  }

  /** The scratch state of the type operations of one thread, other than the guards in `frame`.
   *
   *  A universe used from one thread at a time (the compiler, a toolbox) has one instance; a
   *  concurrent one has one per thread. Anything that must not be seen by an operation on another
   *  thread belongs here, not in a field of `SymbolTable`: anything that is shared, such as caches,
   *  does not.
   */
  final class Scratch {
    /** The frame of the completion in progress on this thread. */
    var frame: Frame = new Frame
    /** The current skolemization level, needed for the algorithms
     *  in isSameType, isSubType that do constraint solving under a prefix.
     */
    var skolemizationLevel: Int = 0
    /** The current indentation string for traces */
    var indent: String = ""
    var toStringRecursions: Int = 0
    val toStringSubjects = new mutable.HashSet[Type]
    /** The recursion depth of locked symbols, see `-Yrecursion` */
    var recursionTable = Map.empty[Symbol, Int]
    /** The number of Namer completers in progress, see `Symbols.lockedCount` */
    var lockedCount: Int = 0
    /** The phase and period of this thread, when they are kept per thread (`phasePerThread`). */
    var ph: Phase = NoPhase
    var per: Period = NoPeriod

    // Reusable instances of helpers with mutable state, one per level of reentrance. As the
    // `Scratch` is confined to a thread, they are reused in runtime reflection too.
    val findMembers = ReusableInstance(new FindMember)
    val containsCollectors = ReusableInstance(new ContainsCollector(null))
    val varianceInTypes = ReusableInstance(new varianceInType)
    val substSymMaps = ReusableInstance(SubstSymMap())
    val onlyChildAccumulators = ReusableInstance(new OnlyChildAccumulator)
    val setChildrenPosAccumulators = ReusableInstance(new SetChildrenPosAccumulator)
    val commonOwnerMaps = ReusableInstance(new CommonOwnerMap)

    // Instances of the type maps and collectors that `TypeMaps` also offers as shared objects.
    // They keep state in fields, but save and restore it on reentrance, so one per thread will do.
    val erroneousCollector = new ErroneousCollector
    val isDependentCollector = new IsDependentCollector
    val isRelatableCollector = new IsRelatableCollector
    val unrelatableCollector = new UnrelatableCollector
    val wildcardExtrapolation = new wildcardExtrapolation
    // OPT: hoist allocation of the collector and lambda out of the loop in `maxTypes`
    val typeContainsTypeVarCollector = new FindTypeCollector(_.isInstanceOf[TypeVar])
    val isWildCardOrNonGroundTypeVarCollector = new FindTypeCollector({
      case tv: TypeVar => !tv.isGround
      case t => t.isWildcard
    })
  }
  private[this] lazy val scratches: Scratches = if (isConcurrentUniverse) new PerThreadScratches else new OneScratch
  final def scratch: Scratch = scratches.get

  private[internal] abstract class Scratches {
    def get: Scratch
  }
  private final class OneScratch extends Scratches {
    private[this] val scratch = new Scratch
    def get: Scratch = scratch
  }
  /** One `Scratch` per thread. The universe holds them, weakly keyed by thread; a `ThreadLocal`
   *  caches a weak reference for the fast path. Holding the `Scratch` itself in the `ThreadLocal`
   *  would leak: its outer pointer reaches this universe and so the `ThreadLocal` key, so a
   *  long-lived thread would keep every universe it ever used.
   */
  private final class PerThreadScratches extends Scratches {
    private[this] val scratches = java.util.Collections.synchronizedMap(new java.util.WeakHashMap[Thread, Scratch])
    private[this] val scratchRef = new ThreadLocal[java.lang.ref.WeakReference[Scratch]]
    def get: Scratch = {
      val ref = scratchRef.get
      val s = if (ref eq null) null else ref.get
      if (s ne null) s else newScratch()
    }
    private def newScratch(): Scratch = {
      val s = new Scratch
      scratches.put(Thread.currentThread, s)
      scratchRef.set(new java.lang.ref.WeakReference(s))
      s
    }
  }

  @deprecated("use enteringPhase", "2.10.0") // Used in sbt 0.12.4
  @inline final def atPhase[T](ph: Phase)(op: => T): T = enteringPhase(ph)(op)

  protected[scala] def currentRunProfilerBeforeCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
  protected[scala] def currentRunProfilerAfterCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
}

trait SymbolTableStats {
  self: TypesStats with Statistics =>

  // Defined here because `SymbolLoaders` is defined in `scala.tools.nsc`
  // and only has access to the `statistics` definition from `scala.reflect`.
  val classReadNanos = newSubTimer("time classfilereading", typerNanos)
}
