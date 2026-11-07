//> using scala 2.13.19-M1
//> using dep org.scala-lang:scala-compiler:2.13.19-M1
//> using javaOpt -Xmx8g -Xss16m

// Extracts the mutable state of the Scala 2 compiler (src/reflect and src/compiler) into a JSON model: per field its
// kind (A scratch, B lazy fact with a black hole, C cache, D interning and counters, E tree and run state, L lazy val),
// the discipline that protects it from concurrent threads, its reach (typer or post-pickler), the instance it lives on
// (universe singleton, per symbol, per type, ...), hazard tags, and every read and write site. Every placement is by a
// rule listed in `Rules` and in the JSON's header, or by `annotations.tsv`; a field no rule places is `unclassified`.
// See README.md.
//
// It runs a `Global` through `typer` over the compiler's own sources and walks the typed trees.

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter

final case class Rule(id: String, sets: String, doc: String)

object Rules {
  val kind: List[Rule] = List(
    Rule("annotations.tsv", "A, B, C, D, E", "a row of `tools/state-map/annotations.tsv` for the field (owner, field, kind, protection, note): takes precedence over every rule below (`kindSource: annotated`); the rules' answer is kept as `inferredKind`"),
    Rule("L.lazy", "L", "a `lazy val` member of a class, trait or object: its initialiser runs under the instance's monitor (`this.synchronized`), so a lazy val whose initialiser forces symbol completion nests a monitor inside whatever the completion takes"),
    Rule("A.scratch", "A", "a field of `SymbolTable#Scratch`, `Frame`, `Scratches` or `ReusableInstance` (retronym/scala#212): one thread's, or one completion level's, scratch"),
    Rule("A.traversal", "A", "a field of a `TypeMap`, `TypeFolder`/`TypeTraverser`/`TypeCollector`, tree `Traverser`/`Transformer`, `FindMemberBase`, or another per-operation helper: one traversal's scratch"),
    Rule("A.typer", "A", "a field of `Typer`, `Context`, `Inferencer`, `ImplicitSearch`, `Namer`, `MacroExpander` or a trait mixed into one of them: one typer's or one context's state (contexts are captured by completers, see the hazard notes)"),
    Rule("A.inference", "A", "a field of `TypeVar`, `TypeConstraint` or `UndoLog`: the state of one inference"),
    Rule("A.iterator", "A", "a field of an `Iterator`: one traversal's"),
    Rule("A.macro", "A", "a field of a class in `reflect.reify`, `reflect.quasiquotes`, `reflect.macros.contexts` or the string interpolators, not a universe singleton: one macro expansion's state"),
    Rule("A.parser", "A","a field of a class in `nsc.ast.parser`, `nsc.javac` or a `CharArrayReader`: one unit's parser or scanner"),
    Rule("B.symbol", "B", "`_rawowner`, `_rawname`, `_rawflags`, `infos`, `_validTo`, `_annotations`, `_privateWithin` and the other own state of a `Symbol` (not its caches), and the `TypeHistory` chain of its infos: written by completion and by info transformers, read everywhere; `_rawflags` carries the `LOCKED` black hole"),
    Rule("B.completer", "B", "a field of a `LazyType` (Namers' completers, `SymbolLoaders.SymbolLoader`, unpickler completers) or of `LazyAnnotationInfo`"),
    Rule("B.unpickler", "B", "a field of a class in `internal.pickling`, `nsc.symtab.classfile`, `nsc.tasty` or `SymbolLoaders`: lazy reads of pickles, classfiles and TASTy"),
    Rule("B.scope", "B", "a field of `Scopes.Scope` (`elems`, the hash table, `elemsCache`): completions enter into scopes that other code iterates"),
    Rule("E.ambient", "E", "a `var` or mutable collection on a per-universe singleton (a cake slice of `SymbolTable`/`Global`, or an object in one) named `ph`, `per`, `phase…`, `globalPhase`, `cur…`, `current…`, `last…`, `phStack`, `infoTransformers`, `nextFrom`, `…Depth`, `…Level`, `…Mode`, `…Stack`, `…Indent`, `…InProgress`, `…Enabled`: a register that assumes one thread runs the universe (tried before the C and D rules)"),
    Rule("C.stamp", "C","declared `Period` or `RunId`, or named `…Period`, `…RunId`, `…ValidTo`, `…validForPeriod`: a stamp, paired with the cache it validates"),
    Rule("C.type-memo", "C", "any other field of a `Type` other than a `TypeVar`: types are hash-consed values, so a field on one memoises a function of the type"),
    Rule("C.bts", "C", "a field of a `BaseTypeSeq` (its elements, the `pending` bit set of the cycle check): the value of a type's base type sequence cache"),
    Rule("C.per-run", "C", "initialised by `perRunCaches.newMap/newSet/newWeakMap/newAnyRefMap/newGeneric/…`: a cache cleared at the end of each run"),
    Rule("C.name", "C", "named `…cache…`/`…Cache…`, `cached…`, `…Memo…`, `last[A-Z]…`: a cache by its name"),
    Rule("D.uniques", "D", "a field of the `Uniques` classes or `WeakHashSet`, or declared one: the hash-consing table of types"),
    Rule("D.names", "D", "a field of `internal.Names` (the name table: `_chrs`, `nc`, the hash tables)"),
    Rule("D.counter", "D", "declared `IdCounter`, a field of a `FreshNameCreator`, or named `…Id`, `ids`, `…Ids`, `…counter`, `cnt`, `nextId…`, `…Serial…`: a source of fresh numbers"),
    Rule("E.tree", "E", "a field of `Trees.Tree` or `StdAttachments.Attachable` (`rawtpe`, `rawatt`, `pos`, the symbol of a `SymTree`)"),
    Rule("E.unit", "E", "a field of `CompilationUnit` or a source file: per unit"),
    Rule("E.run", "E", "a field of `Global#Run`, `Reporting`/`PerRunReporting`, a `Reporter`, `Statistics` or `nsc.profile`: per-run bookkeeping (units, warnings, suppressions, counts)"),
    Rule("E.settings", "E", "a field of `internal.settings` or `nsc.settings`: written while the command line is processed"),
    Rule("E.universe", "E", "any other `var` or mutable collection on a per-universe singleton: global state of the universe (flags, tables)"),
    Rule("A.typer", "A", "(last) any other field of a class (not a trait) in `nsc.typechecker` that is not a universe singleton: a per-operation helper of the typer (`SymbolLookup`, `ImportCursor`, checkers)")
  )

  val protection: List[Rule] = List(
    Rule("annotations.tsv", "any", "the protection column of `annotations.tsv` takes precedence (`protectionSource: annotated`); the rules' answer is kept as `inferredProtection`"),
    Rule("P.thread-local", "thread-confined", "declared `ThreadLocal`"),
    Rule("P.scratch", "thread-confined", "kind `A.scratch`: `SymbolTable#Scratch` is one per thread in a concurrent universe (retronym/scala#212)"),
    Rule("P.confined-owner", "thread-confined", "kind A: the owner is one traversal's, one typer's or one inference's (see the `shared-singleton` hazard for owners also instantiated as universe singletons)"),
    Rule("P.post-pickler", "post-pickler", "reach `post-pickler`: written only by phases after pickler, out of scope for a parallel typer"),
    Rule("P.runtime", "runtime-reflection", "reach `runtime`: runtime reflection (`scala.reflect.runtime`), which has its own GIL and is not part of scalac"),
    Rule("P.driver", "driver", "reach `driver`: the compile server, the command line and the class path resolution, run before or around a compilation"),
    Rule("P.volatile", "volatile", "`@volatile`"),
    Rule("P.concurrent-type", "concurrent", "declared a `java.util.concurrent` or `scala.collection.concurrent` type or an atomic, or initialised with `Collections.synchronized…`"),
    Rule("P.seam", "seam", "declared `IdCounter`, or initialised by an expression that tests `isConcurrentUniverse` or `synchronizeNames`: the universe picks a thread-safe implementation when it is concurrent"),
    Rule("P.lazy", "lazy-monitor", "a `lazy val`: its initialisation holds the owner's monitor; its publication is the lazy val's (a non-volatile bitmap read on the fast path)"),
    Rule("P.init-only", "init-only", "no write (assignment, setter call or mutating call on a collection) outside initialisation: the field is fixed once its owner is constructed, safe if the owner is published safely"),
    Rule("P.access", "(per access)", "every access outside initialisation (constructors, field initialisers), each classified innermost-first: in `x.synchronized { … }` (or its receiver) → synchronized; in an argument of a method that runs that argument only under `synchronized` (found by a first pass, e.g. the name table's `lock`) → synchronized; in `gilSynchronized` → gil; in the else branch of `if (isConcurrentUniverse)` / `if (synchronizeNames)` → batch; in a private or local method, the one protection that every call of it has; else none. The field: the one protection of all non-batch accesses; `batch-only`; `partial:P` if some are P and some none; `mixed:P,Q`; `unknown` if no access outside initialisation is seen")
  )

  val reach: List[Rule] = List(
    Rule("R.runtime", "runtime", "the owner is in `scala.reflect.runtime`"),
    Rule("R.driver", "driver", "the owner is in `nsc.CompileServer`/`CompileSocket`/`CompileClient`/`StandardCompileServer`, `nsc.fsc`, `scala.tools.cmd`, `scala.tools.util`, `nsc.classpath`, `nsc.settings`, `nsc.io`, `nsc.MainClass`/`ScriptRunner`/`PipelineMainClass`, `nsc.plugins`, `nsc.DependencyGraph`, the interactive `WorkScheduler`, or the ToolBox (`tools.reflect.ToolBoxFactory`, `FrontEnd`): run before or around a compilation, or not by scalac"),
    Rule("R.post-pickler", "post-pickler", "the owner is in `nsc.transform` (but `ExtensionMethods`, `Transform`, `TypingTransformers`), `nsc.backend`, `nsc.transform.patmat`, or is `RefChecks`, `Duplicators`, `TreeCheckers`, `Delambdafy`… (classes after pickler); or every access outside initialisation is in such an owner"),
    Rule("R.typer", "typer", "everything else: reachable from parser, namer, packageobjects, typer, superaccessors, extmethods or pickler")
  )

  val excluded: List[Rule] = List(
    Rule("X.collection", "excluded", "the internals of `internal.util.WeakHashSet`, `HashSet`, `Set`, `Collections` and `IdCounter`: modelled where the collection or counter is held")
  )

  val hazard: List[(String, String)] = List(
    "ambient" -> "a global register (phase, run, unit…) that assumes one thread: the first thing that must become per-thread",
    "shared-scratch" -> "scratch state on a per-universe singleton: every thread uses the same instance",
    "shared-singleton" -> "a per-instance class (kind A) that is also instantiated as a universe singleton (`object typer`, an `object` TypeMap): that one instance is shared",
    "context-escape" -> "a field of a typer `Context` written after construction: completers capture contexts and run on whichever thread forces the symbol",
    "black-hole" -> "a lazy fact with a black hole (kind B): completion, LOCKED, loaders, scopes",
    "sentinel" -> "written in a method that throws a cyclic-reference error, or (kinds B and C) read in one: the field doubles as an in-progress marker, so another thread reads 'in progress' as 'cycle'",
    "rmw" -> "a non-atomic read-modify-write (`x += 1`, `flags |= m`, `xs ::= x`): updates are lost under contention",
    "stamp-pair" -> "a stamp and the value it validates in separate fields: a reader can pair one writer's stamp with another's value",
    "collection" -> "an unsynchronised mutable collection: a concurrent update corrupts it",
    "lazy-monitor" -> "a lazy val whose initialiser can force symbol completion: its monitor nests with completion, a deadlock if another thread completes the other way round"
  )

  val feature: List[(String, String)] = List(
    "symbols" -> "`internal.Symbols`, `internal.Scopes`, `internal.Flags`",
    "types" -> "`internal.Types`, `internal.tpe`, `internal.BaseTypeSeqs`",
    "typer" -> "`nsc.typechecker`",
    "implicits" -> "a class named `…Implicit…`",
    "macros" -> "a class named `…Macro…`, or `scala.reflect.macros`, `nsc.typechecker.Macros`",
    "classfile-tasty" -> "`nsc.symtab.classfile`, `nsc.tasty`, `internal.pickling`, `SymbolLoaders`",
    "reporting" -> "`nsc.Reporting`, `nsc.reporters`, `internal.Reporting`",
    "names" -> "`internal.Names`, `internal.StdNames`, `FreshNameCreator`",
    "trees" -> "`internal.Trees`, `nsc.ast`, `internal.Positions`",
    "definitions" -> "`internal.Definitions`, `internal.Mirrors`",
    "settings" -> "`nsc.settings`, `internal.settings`"
  )
}

final case class FieldInfo(
    key: String, owner: String, name: String, tpe: String, file: String, line: Int,
    isVar: Boolean, isLazy: Boolean, isPrivate: Boolean, mutableCollection: Option[String], typeClasses: List[String],
    isVolatile: Boolean, threadLocal: Boolean, concurrentType: Boolean, seam: Boolean, perRun: Boolean,
    forcesCompletion: Boolean, lazyCalls: List[String])

final case class ClassInfo(name: String, simple: String, pkg: String, file: String, line: Int, bases: List[String],
    isModule: Boolean, isTrait: Boolean, isTopLevel: Boolean, var instance: String = "instance", var sharedVia: List[String] = Nil)

final case class Access(field: String, write: Boolean, rmw: Boolean, mutate: Boolean, prot: String, method: String,
    cls: String, init: Boolean, file: String, line: Int)

/** One typed compilation of the compiler's sources, and the facts the model needs from it. */
final class Extractor(val g: Global, repo: Path) {
  import g._

  val classes = mutable.LinkedHashMap[String, ClassInfo]()
  val fields = mutable.LinkedHashMap[String, FieldInfo]()
  val accesses = mutable.ArrayBuffer[Access]()
  // calls of private and local methods: (callee, caller, protection at the call)
  val calls = mutable.ArrayBuffer[(String, String, String)]()
  // methods that throw a cyclic-reference error
  val cyclicMethods = mutable.Set[String]()
  // methods that run a by-name or function parameter only under a protection: method symbol -> (protection, param indices)
  val protecting = mutable.Map[Symbol, (String, Set[Int])]()
  val classSyms = mutable.LinkedHashMap[String, Symbol]()

  def norm(sym: Symbol): String = {
    val fn = sym.fullName
    val t = fn
      .replace("scala.tools.nsc.", "nsc.")
      .replace("scala.reflect.internal.", "internal.")
      .replace("scala.reflect.runtime.", "runtime.")
      .replace("scala.reflect.", "reflect.")
      .replace("scala.tools.", "tools.")
    // anonymous classes of one owner share a name: tell them apart by their line
    if (sym.isModuleClass) t + "$" else if (sym.isAnonymousClass && sym.pos.isDefined) t + "@" + sym.pos.line else t
  }

  def relPath(pos: Position): String =
    if (!pos.isDefined) "" else {
      val p = pos.source.file.path
      val abs = Paths.get(p)
      (if (abs.isAbsolute) repo.relativize(abs).toString else p).replace('\\', '/')
    }

  def inSources(sym: Symbol): Boolean = {
    val f = sym.sourceFile
    f != null && f.path.contains("/src/")
  }

  def fieldName(sym: Symbol): String = {
    val n = sym.name.decoded.trim
    if (sym.isSetter) n.stripSuffix("_=") else n
  }

  /** The field a symbol refers to: a field, its getter or setter, a lazy accessor. */
  def fieldKeyOf(sym: Symbol): Option[String] = {
    if (sym == null || sym == NoSymbol || !sym.isTerm || sym.isModule || sym.isPackage) None
    else {
      val owner = sym.owner
      if (!owner.isClass || owner.isPackageClass) None
      else if (sym.isSetter || sym.isGetter || (sym.isLazy && sym.isMethod)) Some(norm(owner) + "#" + fieldName(sym))
      else if (!sym.isMethod && sym.isValue) Some(norm(owner) + "#" + fieldName(sym))
      else None
    }
  }

  def methodKey(sym: Symbol): String =
    if (sym == null || sym == NoSymbol) "" else norm(sym.owner) + "." + sym.name.decoded.trim

  // ---- types
  val mutablePrefixes = List("scala.collection.mutable.", "scala.collection.concurrent.", "java.util.concurrent.")
  val mutableExact = Set("java.util.Collection", "java.util.Map", "java.util.BitSet", "java.util.Dictionary",
    "java.lang.ThreadLocal", "java.lang.StringBuilder", "java.lang.StringBuffer", "scala.Array",
    "scala.reflect.internal.util.WeakHashSet", "scala.reflect.internal.Scopes.Scope", "scala.reflect.internal.util.IdCounter",
    "scala.reflect.internal.util.ReusableInstance", "scala.reflect.internal.tpe.TypeConstraints.UndoLog",
    "scala.reflect.internal.util.Statistics.Counter", "scala.reflect.internal.util.Statistics.Timer",
    "scala.reflect.internal.util.Statistics.QuantMap", "scala.reflect.internal.util.Statistics.SubCounter",
    "scala.reflect.internal.util.Statistics.StackableTimer", "scala.reflect.internal.util.Statistics.TimerStack")
  val concurrentPrefixes = List("java.util.concurrent.", "scala.collection.concurrent.", "java.lang.ThreadLocal")

  def classesOf(tp: Type): List[Symbol] = {
    val b = List.newBuilder[Symbol]
    tp.foreach { t => val s = t.typeSymbol; if (s.isClass && !s.isPackageClass) b += s }
    b.result().distinct
  }

  def mutableCollection(tp: Type): Option[String] = {
    val t = tp.dealiasWiden
    val s = t.typeSymbol
    if (s == definitions.ArrayClass) Some("scala.Array")
    else if (!s.isClass) None
    else t.baseClasses.find { b => val n = b.fullName; mutablePrefixes.exists(n.startsWith) || mutableExact(n) }
      .map(_ => s.fullName)
  }

  def isConcurrentType(tp: Type): Boolean =
    tp.dealiasWiden.baseClasses.exists(b => concurrentPrefixes.exists(b.fullName.startsWith))

  // ---- names
  val seamNames = Set("isConcurrentUniverse", "synchronizeNames")
  val perRunMakers = Set("newMap", "newSet", "newWeakMap", "newAnyRefMap", "newGeneric", "newWeakSet", "recordCache")
  val mutators = Set("+=", "++=", "-=", "--=", "update", "put", "remove", "clear", "getOrElseUpdate", "addOne", "addAll",
    "subtractOne", "subtractAll", "append", "appendAll", "prepend", "prependAll", "insert", "insertAll", "+=:", "++=:",
    "add", "set", "incrementAndGet", "getAndIncrement", "decrementAndGet", "getAndDecrement", "addAndGet", "getAndAdd",
    "compareAndSet", "getAndSet", "lazySet", "enter", "enterUnique", "enterIfNew", "unlink", "rehash", "putIfAbsent",
    "computeIfAbsent", "computeIfPresent", "compute", "merge", "putAll", "removeAll", "retainAll", "filterInPlace",
    "mapValuesInPlace", "sortInPlace", "reduceToSize", "trimEnd", "trimStart", "dropInPlace", "takeInPlace", "push", "pop",
    "dequeue", "enqueue", "dequeueAll", "setLength", "delete", "deleteCharAt", "addString", "patchInPlace", "flipInPlace",
    "sizeHint", "addEntry", "removeEntry", "clearAndShrink", "subtract", "restore", "undoUnless", "undo", "record",
    "start", "stop", "push", "pop", "value_=", "fill", "reset", "foreachEntry")
  // members whose call can run a completer (forces `info`) or reads a definitions lazy val
  val forcing = Set("info", "tpe", "tpe_*", "tpeHK", "initialize", "baseClasses", "baseType", "baseTypeSeq", "typeSignature",
    "typeParams", "typeConstructor", "parents", "decls", "members", "decl", "member", "nonPrivateMember", "companionModule",
    "companionClass", "companionSymbol", "linkedClassOfClass", "getClass", "getRequiredClass", "getModule", "getRequiredModule",
    "getMember", "getMemberMethod", "getMemberValue", "getMemberClass", "getMemberModule", "getMemberIfDefined",
    "getClassIfDefined", "getModuleIfDefined", "requiredClass", "requiredModule", "getPackage", "getPackageObject",
    "getPackageObjectIfDefined", "staticClass", "staticModule", "typeOfThis", "selfType", "typeOf", "symbolOf",
    "isSubClass", "<:<", "=:=", "weak_<:<", "lub", "glb", "existentialAbstraction", "unsafeTypeParams", "rawInfo_force",
    "sourceModule", "moduleClass", "thisType", "alternatives", "suchThat", "filter", "paramss", "resultType", "finalResultType",
    "asSeenFrom", "memberType", "memberInfo", "isMonomorphicType", "dealias", "normalize", "erasure", "termMember", "typeMember")

  def isSeamCond(t: Tree): Option[Boolean] = t match {
    case Select(q, n) if n.decoded == "unary_!" => isSeamCond(q).map(!_)
    case Apply(Select(q, n), Nil) if n.decoded == "unary_!" => isSeamCond(q).map(!_)
    case Select(_, n) if seamNames(n.decoded) => Some(true)
    case Ident(n) if seamNames(n.decoded) => Some(true)
    case Apply(f, Nil) => isSeamCond(f)
    case _ => None
  }

  def refersTo(t: Tree, names: Set[String]): Boolean =
    t.exists { case s: RefTree => names(s.name.decoded); case _ => false }

  def flatten(t: Tree): (Tree, List[List[Tree]]) = t match {
    case Apply(f, as) => val (fn, ls) = flatten(f); (fn, ls :+ as)
    case TypeApply(f, _) => flatten(f)
    case _ => (t, Nil)
  }

  def isSynchronized(s: Symbol): Boolean =
    s != NoSymbol && s.name.decoded == "synchronized" && (s.owner == definitions.AnyClass || s.owner == definitions.ObjectClass || s.owner == definitions.AnyRefClass)

  def sourceLines(pos: Position): String =
    if (!pos.isDefined) "" else {
      val l = pos.line
      val src = pos.source
      (math.max(1, l - 2) to l).map(i => try src.lineToString(i - 1) catch { case _: Throwable => "" }).mkString("\n")
    }

  // ---- the walk
  final class Walker(collect: Boolean) extends Traverser {
    var methodStack: List[Symbol] = Nil
    var classStack: List[Symbol] = Nil
    var protStack: List[String] = Nil
    var initDepth = 0
    // phase one: the by-name and function parameters of the method being walked, with the protections at their uses
    var paramUses = mutable.Map[Symbol, List[String]]()
    var params: Map[Symbol, Int] = Map.empty

    def prot: String = protStack.headOption.getOrElse("none")
    def withProt[T](p: String)(op: => T): T = { protStack = p :: protStack; try op finally protStack = protStack.tail }

    def currentMethod: String = methodStack.headOption.map(methodKey).getOrElse(classStack.headOption.map(c => norm(c) + ".<init>").getOrElse(""))
    def currentClass: String = classStack.headOption.map(norm).getOrElse("")

    def record(sym: Symbol, write: Boolean, rmw: Boolean, mutate: Boolean, pos: Position): Unit =
      if (collect) fieldKeyOf(sym).foreach { key =>
        accesses += Access(key, write, rmw, mutate, prot, currentMethod, currentClass, initDepth > 0, relPath(pos), if (pos.isDefined) pos.line else 0)
      }

    def recordCall(s: Symbol): Unit =
      if (collect && s != null && s != NoSymbol && s.isMethod && !s.isGetter && !s.isSetter
          && (s.isPrivate || s.isLocalToBlock) && !s.isConstructor)
        calls += ((methodKey(s), currentMethod, prot))

    def recordClass(cls: Symbol, tree: Tree): Unit = if (collect) {
      val name = norm(cls)
      val pkg = norm(cls.enclosingPackageClass).stripSuffix("$")
      classSyms(name) = cls
      classes(name) = ClassInfo(name, cls.name.decoded, pkg, relPath(tree.pos), if (tree.pos.isDefined) tree.pos.line else 0,
        cls.baseClasses.filter(inSources).map(norm), cls.isModuleClass, cls.isTrait, cls.owner.isPackageClass)
    }

    def recordField(vd: ValDef, cls: Symbol): Unit = if (collect) {
      val sym = vd.symbol
      if (sym == NoSymbol || sym.isModule || sym.name.decoded.startsWith("$") || sym.name.decoded.contains("$outer")) return
      val mods = vd.mods
      val isLazy = mods.isLazy || sym.isLazy
      val isVar = !isLazy && (mods.isMutable || sym.isMutable)
      val tpe = if (vd.tpt.tpe != null) vd.tpt.tpe else sym.tpe.resultType
      val coll = mutableCollection(tpe)
      if (!(isVar || isLazy || coll.isDefined)) return
      if (sym.isSynthetic && !isLazy) return
      val key = norm(cls) + "#" + fieldName(sym)
      val text = sourceLines(vd.pos)
      val volatile = sym.hasAnnotation(definitions.VolatileAttr) || {
        val lastLine = text.split('\n').lastOption.getOrElse("")
        lastLine.contains("@volatile") || text.split('\n').reverse.drop(1).headOption.exists(l => l.trim.startsWith("@volatile") && !l.contains(" val ") && !l.contains(" var "))
      }
      val rhs = vd.rhs
      val seam = refersTo(rhs, seamNames) || tpe.dealiasWiden.typeSymbol.fullName == "scala.reflect.internal.util.IdCounter"
      val perRun = rhs.exists {
        case Select(q, n) => perRunMakers(n.decoded) && q.symbol != null && q.symbol.name.decoded == "perRunCaches"
        case _ => false
      }
      val syncWrapped = rhs.exists { case Select(q, n) => n.decoded.startsWith("synchronized") && q.symbol != null && q.symbol.fullName == "java.util.Collections"; case _ => false }
      val (forces, lazyCalls) =
        if (!isLazy) (false, Nil)
        else {
          val called = mutable.LinkedHashSet[String]()
          var f = false
          rhs.foreach {
            case s @ (_: Select | _: Ident) if s.symbol != null && s.symbol != NoSymbol && s.symbol.isMethod =>
              val ms = s.symbol
              val n = ms.name.decoded
              val own = ms.owner.fullName
              if (forcing(n) && (own.startsWith("scala.reflect") || own.startsWith("scala.tools"))) f = true
              if (ms.isLazy && (own.startsWith("scala.reflect") || own.startsWith("scala.tools"))) f = true
              if (own.startsWith("scala.reflect") || own.startsWith("scala.tools")) called += n
            case _ =>
          }
          (f, called.toList.take(10))
        }
      val tcs = classesOf(tpe).filter(inSources).map(norm)
      val shown = tpe.toString.replace("scala.collection.mutable.", "mutable.").replace("scala.reflect.internal.", "").replace("scala.tools.nsc.", "")
      fields(key) = FieldInfo(key, norm(cls), fieldName(sym), shown, relPath(vd.pos), if (vd.pos.isDefined) vd.pos.line else 0,
        isVar, isLazy, sym.isPrivate || mods.isPrivate, coll, tcs, volatile, tpe.dealiasWiden.baseClasses.exists(_.fullName == "java.lang.ThreadLocal"),
        isConcurrentType(tpe) || syncWrapped, seam, perRun, forces, lazyCalls)
    }

    // does `t` read the field `key` of the same receiver as `recv` (a read-modify-write, not a copy from another instance)
    def readsKey(t: Tree, key: String, recv: Option[Tree]): Boolean = {
      def sameRecv(q: Option[Tree]) = (q, recv) match {
        case (Some(_: This), Some(_: This)) | (None, None) | (Some(_: This), None) | (None, Some(_: This)) => true
        case (Some(a), Some(b)) => a.equalsStructure(b)
        case _ => false
      }
      t.exists {
        case s @ Select(q, _) => s.symbol != null && fieldKeyOf(s.symbol).contains(key) && sameRecv(Some(q))
        case i: Ident => i.symbol != null && fieldKeyOf(i.symbol).contains(key) && sameRecv(None)
        case _ => false
      }
    }
    def receiver(t: Tree): Option[Tree] = t match { case Select(q, _) => Some(q); case _ => None }

    override def traverse(tree: Tree): Unit = tree match {
      case impl: ImplDef =>
        val cls = if (impl.symbol.isModule) impl.symbol.moduleClass else impl.symbol
        if (cls == NoSymbol) return
        recordClass(cls, impl)
        impl.impl.body.foreach { case vd: ValDef => recordField(vd, cls); case _ => }
        val (sm, sc, si) = (methodStack, classStack, initDepth)
        methodStack = Nil; classStack = cls :: classStack; initDepth = 1
        impl.impl.parents.foreach(traverse)
        impl.impl.body.foreach {
          case d: DefDef => initDepth = if (d.symbol.isConstructor) 1 else 0; traverse(d); initDepth = 1
          case c: ImplDef => traverse(c)
          case vd: ValDef if vd.mods.isLazy || vd.symbol.isLazy =>
            initDepth = 0; methodStack = vd.symbol :: methodStack; traverse(vd.rhs); methodStack = methodStack.tail; initDepth = 1
          case t => traverse(t)
        }
        methodStack = sm; classStack = sc; initDepth = si
      case dd: DefDef =>
        val (savedParams, savedUses) = (params, paramUses)
        if (!collect) {
          val ps = dd.vparamss.flatten.zipWithIndex.collect {
            case (p, i) if p.symbol != NoSymbol && (definitions.isByNameParamType(p.symbol.tpe) || definitions.isFunctionType(p.symbol.tpe)) => p.symbol -> i
          }.toMap
          params = ps; paramUses = mutable.Map()
        }
        methodStack = dd.symbol :: methodStack
        val savedProt = protStack
        protStack = Nil
        val savedInit = initDepth
        if (!dd.symbol.isConstructor) initDepth = 0
        try {
          dd.vparamss.flatten.foreach(p => traverse(p.rhs))
          traverse(dd.rhs)
          if (collect && dd.rhs.exists {
            case Throw(e) => e.tpe != null && e.tpe.typeSymbol.name.decoded.contains("Cyclic")
            case _ => false
          }) cyclicMethods += methodKey(dd.symbol)
        } finally { methodStack = methodStack.tail; protStack = savedProt; initDepth = savedInit }
        if (!collect) {
          val prots = for ((p, i) <- params.toList; uses <- paramUses.get(p).toList) yield (i, uses)
          val protected0 = prots.filter { case (_, us) => us.nonEmpty && us.exists(_ != "batch") && us.forall(u => u == "synchronized" || u == "batch") }
          if (protected0.nonEmpty) protecting(dd.symbol) = ("synchronized", protected0.map(_._1).toSet)
          params = savedParams; paramUses = savedUses
        }
      case Assign(lhs, rhs) =>
        lhs.symbol match {
          case null =>
          case s => fieldKeyOf(s).foreach(k => record(s, write = true, rmw = readsKey(rhs, k, receiver(lhs)), mutate = false, lhs.pos))
        }
        lhs match { case Select(q, _) => traverse(q); case _ => }
        traverse(rhs)
      case If(cond, thenp, elsep) if isSeamCond(cond).isDefined =>
        traverse(cond)
        val positive = isSeamCond(cond).get
        withProt(if (positive) "concurrent-branch" else "batch")(traverse(thenp))
        withProt(if (positive) "batch" else "concurrent-branch")(traverse(elsep))
      case app: Apply =>
        val (fn, argss) = flatten(app)
        val fsym = fn.symbol
        val allArgs = argss.flatten
        if (fsym != null && fsym.isSetter) {
          fieldKeyOf(fsym).foreach(k => record(fsym, write = true, rmw = allArgs.exists(readsKey(_, k, receiver(fn))), mutate = false, fn.pos))
          fn match { case Select(q, _) => traverse(q); case _ => }
          allArgs.foreach(traverse)
        } else if (fsym != null && isSynchronized(fsym)) {
          fn match { case Select(q, _) => withProt("synchronized")(traverse(q)); case _ => }
          allArgs.foreach(a => withProt("synchronized")(traverse(a)))
        } else if (fsym != null && (fsym.name.decoded == "gilSynchronized" || fsym.name.decoded == "gilSynchronizedIfNotThreadsafe")) {
          traverse(fn)
          allArgs.foreach(a => withProt("gil")(traverse(a)))
        } else if (fsym != null && protecting.contains(fsym)) {
          val (p, idxs) = protecting(fsym)
          traverse(fn)
          argss.headOption.getOrElse(Nil).zipWithIndex.foreach { case (a, i) => if (idxs(i)) withProt(p)(traverse(a)) else traverse(a) }
          argss.drop(1).flatten.foreach(traverse)
        } else {
          fn match {
            case Select(recv, n) if mutators(n.decoded) && recv.symbol != null && fieldKeyOf(recv.symbol).isDefined
                && (recv.isInstanceOf[Select] || recv.isInstanceOf[Ident]) =>
              val k = fieldKeyOf(recv.symbol).get
              record(recv.symbol, write = true, rmw = false, mutate = true, recv.pos)
              recv match { case Select(q, _) => traverse(q); case _ => }
              recordCall(fsym)
              allArgs.foreach(traverse)
            case _ =>
              recordCall(fsym)
              super.traverse(tree)
          }
        }
      case s @ Select(q, _) =>
        recordCall(s.symbol)
        record(s.symbol, write = false, rmw = false, mutate = false, s.pos)
        traverse(q)
      case i @ Ident(_) =>
        if (!collect && params.contains(i.symbol)) paramUses(i.symbol) = prot :: paramUses.getOrElse(i.symbol, Nil)
        recordCall(i.symbol)
        record(i.symbol, write = false, rmw = false, mutate = false, i.pos)
      case _ => super.traverse(tree)
    }
  }

  def collect(): Unit = {
    val units = currentRun.units.toList
    val p1 = new Walker(collect = false)
    units.foreach(u => p1.traverse(u.body))
    val p2 = new Walker(collect = true)
    units.foreach { u =>
      try p2.traverse(u.body)
      catch { case e: Throwable => System.err.println(s"skipped part of ${u.source.file.path}: $e") }
    }
    computeInstances()
  }

  // ---- what each class's instances are
  def computeInstances(): Unit = {
    def cls(n: String) = rootMirror.getClassIfDefined(n)
    val roots = List("scala.tools.nsc.Global", "scala.reflect.internal.SymbolTable").map(cls).filter(_ != NoSymbol)
    val universe = mutable.LinkedHashSet[Symbol]()
    val singletons = mutable.LinkedHashSet[Symbol]()
    // a slice of the universe: its fields are per universe, and so are the objects it declares
    // every concrete subclass is an object or an anonymous class: the class is instantiated only as a singleton
    // (`DefinitionsClass`, `Analyzer` in `lazy val analyzer = new { … } with Analyzer`)
    def onlySingletonInstances(b: Symbol): Boolean = {
      val concrete = classes.values.filter(ci => ci.bases.contains(norm(b)) && !ci.isTrait && !classSyms.get(ci.name).exists(_.isAbstractClass))
      concrete.nonEmpty && concrete.forall(ci => ci.isModule || classSyms.get(ci.name).exists(_.isAnonymousClass))
    }
    def addSlice(c: Symbol): Unit =
      if (universe.add(c)) for (d <- c.info.decls.toList) {
        if (d.isModule && !d.isPackage && d.moduleClass != NoSymbol) addSingleton(d.moduleClass)
        // a component held in a val or lazy val of the universe (`lazy val loaders = new { … } with GlobalSymbolLoaders`)
        else if (d.isTerm && !d.isSetter && (d.isLazy || d.isStable) && !d.isModule) {
          val rt = d.tpe.resultType
          if (sys.env.contains("STATE_MAP_DEBUG") && d.name.decoded.trim == sys.env("STATE_MAP_DEBUG"))
            println(s"slice ${norm(c)}.${d.name}: $rt ${rt.baseClasses.map(b => s"${norm(b)} src=${inSources(b)} top=${b.owner.isPackageClass} abs=${b.isTrait || b.isAbstractClass} only=${onlySingletonInstances(b)}")}")
          for (b <- rt.baseClasses if inSources(b) && !universe(b) && b.owner.isPackageClass && (b.isTrait || b.isAbstractClass) && onlySingletonInstances(b))
            addSlice(b)
        }
      }
    def addSingleton(mc: Symbol): Unit =
      if (singletons.add(mc)) {
        addSlice(mc)
        for (bb <- mc.baseClasses.tail if inSources(bb)) {
          // a top-level cake slice mixed into the singleton, or a nested class whose every concrete subclass is a
          // singleton, is part of the universe; any other base (`object typer extends analyzer.Typer`, an `object`
          // TypeMap) is a per-instance class with a shared instance
          if ((bb.owner.isPackageClass && (bb.isTrait || bb.isAbstractClass)) || onlySingletonInstances(bb)) addSlice(bb)
        }
      }
    for (r <- roots; b <- r.baseClasses if inSources(b)) addSlice(b)
    val uNames = (universe ++ singletons).map(norm).toSet
    def derives(ci: ClassInfo, base: String) = ci.name == base || ci.bases.contains(base)
    val perInstance = List(
      "symbol" -> "internal.Symbols.Symbol", "type" -> "internal.Types.Type", "tree" -> "internal.Trees.Tree",
      "scope" -> "internal.Scopes.Scope", "context" -> "nsc.typechecker.Contexts.Context", "typer" -> "nsc.typechecker.Typers.Typer",
      "typer" -> "nsc.typechecker.Infer.Inferencer", "typer" -> "nsc.typechecker.Implicits.ImplicitSearch",
      "typer" -> "nsc.typechecker.Namers.Namer", "completer" -> "internal.Types.LazyType",
      "traversal" -> "internal.tpe.TypeMaps.TypeMap", "traversal" -> "internal.tpe.TypeMaps.TypeFolder",
      "traversal" -> "reflect.api.Trees.Traverser", "traversal" -> "reflect.api.Trees.Transformer",
      "name" -> "internal.Names.Name", "run" -> "nsc.Global.Run", "unit" -> "nsc.CompilationUnits.CompilationUnit",
      "annotation" -> "internal.AnnotationInfos.AnnotationInfo", "position" -> "internal.util.Position",
      "source" -> "internal.util.SourceFile", "phase" -> "internal.Phase")
    for (ci <- classes.values) {
      ci.instance =
        if (uNames(ci.name)) "universe"
        else perInstance.collectFirst { case (k, b) if derives(ci, b) => k }.getOrElse {
          val s = classSyms(ci.name)
          if (s.isModuleClass && s.isStatic) "static" else "instance"
        }
      if (ci.instance != "universe" && ci.instance != "static")
        ci.sharedVia = singletons.toList.filter(s => s.baseClasses.exists(b => norm(b) == ci.name)).map(norm)
    }
  }
}

object StateMap {
  def jstr(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c => sb += c
    }
    sb += '"'
    sb.toString
  }
  def jarr(xs: Iterable[String]): String = xs.mkString("[", ",", "]")
  def jobj(kvs: (String, String)*): String = kvs.map { case (k, v) => s"${jstr(k)}:$v" }.mkString("{", ",", "}")
  def jtop(kvs: (String, String)*): String = kvs.map { case (k, v) => s"${jstr(k)}: $v" }.mkString("{\n", ",\n", "\n}\n")

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val repo = Paths.get(opts.getOrElse("repo", ".")).toAbsolutePath.normalize
    val cp = opts("cp")
    val out = Paths.get(opts.getOrElse("out", "tools/state-map/state-map.json"))
    val tip = opts.getOrElse("tip", "HEAD")
    val gh = opts.getOrElse("github", "https://github.com/retronym/scala/blob")
    val extra = opts.get("opts-file").map(f => Files.readAllLines(Paths.get(f)).asScala.toList.map(_.trim).filter(_.nonEmpty)).getOrElse(Nil)
      .filterNot(o => o == "-Werror" || o == "-Xfatal-warnings")
    val roots = opts.getOrElse("sources", "src/reflect,src/compiler").split(',').toList
    val sources = roots.flatMap { r =>
      Files.walk(repo.resolve(r)).iterator.asScala.map(_.toString).filter(p => p.endsWith(".scala") || p.endsWith(".java")).toList.sorted
    }
    val settings = new Settings(msg => System.err.println(msg))
    val (ok, rest) = settings.processArguments(extra ++ List("-nowarn", "-Ystop-after:typer", "-classpath", cp), processAll = true)
    if (!ok) System.err.println(s"unprocessed options: $rest")
    settings.usejavacp.value = false
    val reporter = new ConsoleReporter(settings)
    val global = new Global(settings, reporter)
    val t0 = System.nanoTime
    val run = new global.Run
    run.compile(sources)
    println(f"typed ${sources.size} sources in ${(System.nanoTime - t0) / 1e9}%.0fs, ${reporter.errorCount} errors")
    val ex = new Extractor(global, repo)
    ex.collect()
    val annotations = {
      val f = repo.resolve("tools/state-map/annotations.tsv")
      if (!Files.exists(f)) Nil
      else Files.readAllLines(f).asScala.toList.filterNot(l => l.startsWith("#") || l.isBlank).map(_.split('\t').toList.padTo(5, ""))
    }
    val model = new Model(ex.classes.toMap, ex.fields.values.toList, ex.accesses.toList, ex.calls.toList, ex.cyclicMethods.toSet, annotations)
    val json = model.toJson(tip, gh)
    Files.createDirectories(out.toAbsolutePath.getParent)
    Files.writeString(out, json)
    val js = out.resolveSibling(out.getFileName.toString.stripSuffix(".json") + ".data.js")
    Files.writeString(js, "window.STATE_MAP = " + model.toJson(tip, gh, sitesCap = 60) + ";\n")
    val wl = out.resolveSibling("worklist.md")
    Files.writeString(wl, model.worklist(tip, gh))
    println(model.summary)
    println(s"wrote ${out.toAbsolutePath}, ${js.toAbsolutePath} and ${wl.toAbsolutePath}")
  }
}

final case class Placed(f: FieldInfo, kind: String, kindRule: String, protection: String, protRule: String,
    evidence: String, features: List[String], reach: String, reachRule: String, instance: String, sharedVia: List[String],
    hazards: List[String], accessSummary: Map[String, Int], edges: List[String], sites: List[String], unprotectedSites: List[String],
    kindSource: String, protectionSource: String, inferredKind: String, inferredKindRule: String,
    inferredProtection: String, inferredProtectionRule: String, note: String, unprotectedCount: Int, writeCount: Int)

final class Model(classes: Map[String, ClassInfo], fieldList: List[FieldInfo], accesses: List[Access],
    calls: List[(String, String, String)], cyclicMethods: Set[String], annotations: List[List[String]]) {
  import StateMap._

  private def cls(n: String) = classes.get(n)
  private def derives(owner: String, base: String) = owner == base || cls(owner).exists(_.bases.contains(base))
  // the owner is `base` or a trait mixed into it
  private def partOf(owner: String, base: String) = derives(owner, base) || cls(base).exists(_.bases.contains(owner)) && cls(owner).exists(_.isTrait) && !cls(owner).exists(_.isTopLevel)
  private def pkgOf(f: FieldInfo) = cls(f.owner).map(_.pkg).getOrElse("")
  private def ownerFile(f: FieldInfo) = cls(f.owner).map(_.file).getOrElse(f.file)

  private val accByField: Map[String, List[Access]] = accesses.groupBy(_.field)
  private val callsTo: Map[String, List[(String, String, String)]] = calls.distinct.groupBy(_._1)

  private def relevant(f: FieldInfo): List[Access] = accByField.getOrElse(f.key, Nil).filterNot(_.init)

  private val methodMemo = mutable.Map[String, String]()
  private def siteProt(prot: String, method: String, seen: Set[String]): String =
    if (prot != "none" && prot != "concurrent-branch") prot else methodProt(method, seen)
  private def methodProt(m: String, seen: Set[String]): String =
    if (seen(m)) "none"
    else methodMemo.getOrElse(m, {
      val sites = callsTo.getOrElse(m, Nil).filter(_._2 != m)
      val ps = sites.map { case (_, caller, p) => siteProt(p, caller, seen + m) }.filterNot(_ == "batch").toSet
      val p = if (sites.isEmpty) "none" else if (ps.isEmpty) "batch" else if (ps.size == 1) ps.head else "none"
      if (seen.isEmpty) methodMemo(m) = p
      p
    })
  private def accessProt(a: Access): String = siteProt(a.prot, a.method, Set.empty)

  // ---- reach
  private val postPicklerPkgs = List("nsc.backend", "nsc.transform")
  private val typerTransforms = Set("nsc.transform.ExtensionMethods", "nsc.transform.Transform", "nsc.transform.TypingTransformers")
  private val postPicklerClasses = List("nsc.typechecker.RefChecks", "nsc.typechecker.Duplicators", "nsc.typechecker.TreeCheckers",
    "nsc.typechecker.SyntheticMethods_post", "nsc.ast.TreeBrowsers", "nsc.ast.NodePrinters", "nsc.Global.Run.trackerFactory_post")
  private def isPostPicklerOwner(o: String): Boolean = {
    val inPkg = postPicklerPkgs.exists(p => o.startsWith(p + ".")) && !typerTransforms.exists(t => o == t || o.startsWith(t + ".") || o.startsWith(t + "$"))
    inPkg || postPicklerClasses.exists(c => o == c || o.startsWith(c + ".") || o.startsWith(c + "$"))
  }
  private val driverPrefixes = List("nsc.CompileServer", "nsc.StandardCompileServer", "nsc.CompileSocket", "nsc.CompileClient",
    "nsc.StandardCompileClient", "nsc.fsc", "tools.cmd", "tools.util", "nsc.classpath", "nsc.settings", "nsc.io", "nsc.MainClass",
    "nsc.ScriptRunner", "nsc.plugins", "nsc.GenericRunnerSettings", "nsc.PipelineMain", "nsc.PipelineMainClass", "nsc.ConsoleWriter",
    "nsc.Driver", "nsc.MainBench", "nsc.Main", "nsc.ClassPathMemoryConsumptionTester", "nsc.DependencyGraph", "nsc.util.WorkScheduler",
    "nsc.util.InterruptReq", "tools.reflect.ToolBoxFactory", "tools.reflect.FrontEnd")

  private def reachOf(f: FieldInfo): (String, String) = {
    val o = f.owner
    if (o.startsWith("runtime.")) ("runtime", "R.runtime")
    else if (driverPrefixes.exists(p => o == p || o.startsWith(p + ".") || o.startsWith(p + "$"))) ("driver", "R.driver")
    else if (isPostPicklerOwner(o)) ("post-pickler", "R.post-pickler")
    else {
      val acc = relevant(f)
      if (acc.nonEmpty && acc.forall(a => isPostPicklerOwner(a.cls))) ("post-pickler", "R.post-pickler")
      else ("typer", "R.typer")
    }
  }

  // ---- kinds
  private val symbolOwn = Set("_rawowner", "_rawname", "_rawflags", "infos", "_validTo", "_annotations", "_privateWithin",
    "_initialized", "_rawinfo", "_associatedFile", "_moduleClass", "_sourceModule", "_referenced", "_children", "_companionModule",
    "_typeOfThis", "_flatname", "_lockedCount", "rawpos", "flatname", "_initBaseTypeSeq", "referenced", "childSet", "_tpeCache")
  private def isStamp(f: FieldInfo) = {
    val t = f.tpe
    Set("Period", "RunId", "Int @Period")(t) && (f.name.toLowerCase.contains("period") || f.name.toLowerCase.contains("run") || f.name.contains("valid")) ||
      f.mutableCollection.isEmpty && (f.name.endsWith("Period") || f.name.endsWith("RunId") || f.name.endsWith("ValidTo") || f.name.endsWith("validTo") || f.name.endsWith("ValidForPeriod"))
  }
  private def onUniverse(f: FieldInfo) = cls(f.owner).exists(c => c.instance == "universe" || c.instance == "static")

  private val typerBases = List("nsc.typechecker.Typers.Typer", "nsc.typechecker.Contexts.Context", "nsc.typechecker.Contexts.ImportInfo",
    "nsc.typechecker.Infer.Inferencer", "nsc.typechecker.Implicits.ImplicitSearch", "nsc.typechecker.Implicits.ImplicitComputation",
    "nsc.typechecker.Namers.Namer", "nsc.typechecker.Macros.MacroExpander", "nsc.typechecker.Contexts.ContextReporter")
  private val traversalBases = List("internal.tpe.TypeMaps.TypeMap", "internal.tpe.TypeMaps.TypeFolder", "reflect.api.Trees.Traverser",
    "reflect.api.Trees.Transformer", "internal.tpe.FindMembers.FindMemberBase", "internal.Trees.InternalTraverser",
    "internal.Printers.TreePrinter", "nsc.ast.Printers.TreePrinter", "internal.tpe.TypeComparers.SubTypePair_none",
    "internal.TreeGen_none", "internal.tpe.TypeToStrings_none", "internal.util.Collections_none")

  private val parserPkgs = List("nsc.ast.parser.", "nsc.javac.", "nsc.util.CharArrayReader", "nsc.util.JavaCharArrayReader")
  private val macroPkgs = List("reflect.reify.", "reflect.quasiquotes.", "tools.reflect.FormatInterpolator", "tools.reflect.FastStringInterpolator", "reflect.macros.contexts.")

  private def ambientName(n: String) =
    Set("ph", "per", "phStack", "globalPhase", "curRun", "curRunId", "lastSeenSourceFile", "lastSeenContext", "phaseWithId",
      "currentRun", "currentUnit", "_currentRun", "_currentUnit", "infoTransformers", "nextFrom", "phaseStack", "globalPhase0")(n) ||
      raw"(?i)_?(cur|current|global|last)[A-Z_]\w*".r.matches(n) || raw"_?phase\w*".r.matches(n) ||
      raw"\w*(Mode|Enabled)".r.matches(n) || raw"(?i)\w*(depth|level|stack|indent|nesting|inprogress)".r.matches(n)

  private def kindOf(f: FieldInfo): (String, String) = {
    val o = f.owner
    def is(base: String) = derives(o, base)
    def part(base: String) = partOf(o, base)
    val file = ownerFile(f)
    if (f.isLazy) ("L", "L.lazy")
    else if (o.startsWith("internal.SymbolTable.Scratch") || o.startsWith("internal.SymbolTable.Frame") || is("internal.SymbolTable.Scratches") || is("internal.util.ReusableInstance")) ("A", "A.scratch")
    else if (traversalBases.exists(is)) ("A", "A.traversal")
    else if (typerBases.exists(part)) ("A", "A.typer")
    else if (is("internal.Types.TypeVar") || is("internal.tpe.TypeConstraints.TypeConstraint") || is("internal.tpe.TypeConstraints.UndoLog")) ("A", "A.inference")
    else if (is("scala.collection.Iterator") || cls(o).exists(_.bases.exists(_.endsWith("Iterator")))) ("A", "A.iterator")
    else if (parserPkgs.exists(p => o.startsWith(p))) ("A", "A.parser")
    else if (macroPkgs.exists(p => o.startsWith(p)) && !onUniverse(f)) ("A", "A.macro")
    else if (is("internal.Symbols.Symbol") && symbolOwn(f.name) || o == "internal.Symbols.TypeHistory") ("B", "B.symbol")
    else if (is("internal.Types.LazyType") || is("internal.AnnotationInfos.LazyAnnotationInfo") || is("nsc.symtab.SymbolLoaders.SymbolLoader")) ("B", "B.completer")
    else if (List("internal.pickling", "nsc.symtab.classfile", "nsc.tasty", "tools.tasty").exists(p => pkgOf(f) == p || pkgOf(f).startsWith(p + ".")) || o.startsWith("nsc.symtab.SymbolLoaders")) ("B", "B.unpickler")
    else if (is("internal.Scopes.Scope") || is("internal.Scopes.ScopeEntry")) ("B", "B.scope")
    else if (onUniverse(f) && (f.isVar || f.mutableCollection.isDefined) && ambientName(f.name)) ("E", "E.ambient")
    else if (isStamp(f)) ("C", "C.stamp")
    else if (is("internal.Types.Type") && !is("internal.Types.TypeVar")) ("C", "C.type-memo")
    else if (is("internal.BaseTypeSeqs.BaseTypeSeq")) ("C", "C.bts")
    else if (f.perRun) ("C", "C.per-run")
    else if (raw"(?i).*(cache|memo).*".r.matches(f.name) || raw"last[A-Z]\w*".r.matches(f.name)) ("C", "C.name")
    else if (file.endsWith("internal/util/WeakHashSet.scala") || o.contains("Uniques") || f.tpe.contains("Uniques") || f.tpe.contains("WeakHashSet")) ("D", "D.uniques")
    else if (file.endsWith("internal/Names.scala")) ("D", "D.names")
    else if (f.tpe.contains("IdCounter") || is("internal.util.FreshNameCreator") || raw"(?i)_?(next\w*id|nextid\w*|\w*ids|\w*counter|\w*cnt|\w*serial\w*|uniqueid\w*)".r.matches(f.name)) ("D", "D.counter")
    else if (is("internal.Trees.Tree") || is("internal.StdAttachments.Attachable")) ("E", "E.tree")
    else if (is("nsc.CompilationUnits.CompilationUnit") || is("internal.util.SourceFile")) ("E", "E.unit")
    else if (o.startsWith("nsc.Global.Run") || o.startsWith("nsc.Reporting") || o.startsWith("nsc.reporters") || o.startsWith("internal.Reporting")
      || is("internal.Reporter") || o.contains("Statistics") || o.startsWith("internal.util.Statistics") || o.startsWith("nsc.profile")) ("E", "E.run")
    else if (o.startsWith("internal.settings") || o.startsWith("nsc.settings")) ("E", "E.settings")
    else if (onUniverse(f)) ("E", "E.universe")
    else if (o.startsWith("nsc.typechecker.") && cls(o).exists(c => !c.isTrait)) ("A", "A.typer")
    else ("unclassified", "")
  }

  private def protOf(f: FieldInfo, kindRule: String, reach: String): (String, String, String) = {
    val acc = relevant(f)
    if (f.threadLocal) ("thread-confined", "P.thread-local", "type")
    else if (kindRule == "A.scratch") ("thread-confined", "P.scratch", "owner")
    else if (kindRule.startsWith("A.") && !onUniverse(f)) ("thread-confined", "P.confined-owner", "owner")
    else if (reach == "post-pickler") ("post-pickler", "P.post-pickler", "owner")
    else if (reach == "runtime") ("runtime-reflection", "P.runtime", "owner")
    else if (reach == "driver") ("driver", "P.driver", "owner")
    else if (f.isVolatile) ("volatile", "P.volatile", "annotation")
    else if (f.concurrentType) ("concurrent", "P.concurrent-type", "type")
    else if (f.seam) ("seam", "P.seam", "lexical")
    else if (f.isLazy) ("lazy-monitor", "P.lazy", "declaration")
    else if (acc.nonEmpty && acc.forall(!_.write)) ("init-only", "P.init-only", "lexical")
    else {
      val ps = acc.map(accessProt)
      val nonBatch = ps.filterNot(_ == "batch").toSet
      val p =
        if (acc.isEmpty) "unknown"
        else if (nonBatch.isEmpty) "batch-only"
        else if (nonBatch.size == 1) nonBatch.head
        else if (nonBatch.contains("none")) "partial:" + (nonBatch - "none").toList.sorted.mkString(",")
        else "mixed:" + nonBatch.toList.sorted.mkString(",")
      (p, "P.access", "lexical")
    }
  }

  private def featuresOf(f: FieldInfo): List[String] = {
    val o = f.owner
    val b = List.newBuilder[String]
    if (o.startsWith("internal.Symbols") || o.startsWith("internal.Scopes") || o.startsWith("internal.Flags")) b += "symbols"
    if (o.startsWith("internal.Types") || o.startsWith("internal.tpe") || o.startsWith("internal.BaseTypeSeqs")) b += "types"
    if (o.startsWith("nsc.typechecker")) b += "typer"
    if (o.contains("Implicit")) b += "implicits"
    if (o.contains("Macro") || o.startsWith("reflect.macros")) b += "macros"
    if (o.startsWith("nsc.symtab.classfile") || o.startsWith("nsc.tasty") || o.startsWith("internal.pickling") || o.contains("SymbolLoaders")) b += "classfile-tasty"
    if (o.startsWith("nsc.Reporting") || o.startsWith("nsc.reporters") || o.startsWith("internal.Reporting")) b += "reporting"
    if (o.startsWith("internal.Names") || o.startsWith("internal.StdNames") || o.contains("FreshNameCreator")) b += "names"
    if (o.startsWith("internal.Trees") || o.startsWith("nsc.ast") || o.startsWith("internal.Positions")) b += "trees"
    if (o.startsWith("internal.Definitions") || o.startsWith("internal.Mirrors")) b += "definitions"
    if (o.startsWith("nsc.settings") || o.startsWith("internal.settings")) b += "settings"
    b.result()
  }

  private def stem(n: String) = n.replaceAll("(Period|RunId|ValidTo|validTo|ValidForPeriod|Cache|cache|Valid|valid)$", "")
  private val stampStems: Set[(String, String)] = fieldList.filter(isStamp).map(f => (f.owner, stem(f.name))).toSet

  private val annByKey: Map[String, List[String]] = annotations.map(r => (r(0) + "#" + r(1)) -> r).toMap

  // the internals of a mutable collection class or a counter: modelled where it is held, not here
  private val collectionInternals = List("internal.util.WeakHashSet", "internal.util.HashSet", "internal.util.Set",
    "internal.util.Collections", "internal.util.IdCounter")
  def excluded(f: FieldInfo) = collectionInternals.exists(c => f.owner == c || f.owner.startsWith(c + ".") || f.owner.startsWith(c + "$"))

  val placed: List[Placed] = fieldList.filterNot(excluded).map { f =>
    val (reach, reachRule) = reachOf(f)
    val (ik, ikr) = kindOf(f)
    val (ip, ipr, iev) = protOf(f, ikr, reach)
    val ann = annByKey.get(f.key)
    val (k, kr, ks) = ann.filter(_(2).nonEmpty).map(r => (r(2), "annotations.tsv", "annotated")).getOrElse((ik, ikr, "inferred"))
    val (p, pr, ev, ps) = ann.filter(_(3).nonEmpty).map(r => (r(3), "annotations.tsv", "annotation", "annotated")).getOrElse((ip, ipr, iev, "inferred"))
    val note = ann.map(_(4)).getOrElse("")
    val acc = relevant(f)
    val ci = cls(f.owner)
    val instance = ci.map(_.instance).getOrElse("instance")
    val sharedVia = ci.map(_.sharedVia).getOrElse(Nil)
    val unprot = acc.filter(a => { val q = accessProt(a); q == "none" || q == "concurrent-branch" })
    val bad = p == "none" || p.startsWith("partial")
    val hz = List.newBuilder[String]
    if (kr == "E.ambient") hz += "ambient"
    if (k == "A" && onUniverse(f)) hz += "shared-scratch"
    if (k == "A" && sharedVia.nonEmpty && reach == "typer") hz += "shared-singleton"
    if (k == "B") hz += "black-hole"
    // an in-progress marker: written in a method that throws a cyclic-reference error, or read in one when the
    // field is a lazy fact or a cache (`baseTypeSeqCache eq undetBaseTypeSeq`)
    if (acc.exists(a => cyclicMethods(a.method) && a.write) || (Set("B", "C")(k) && acc.exists(a => cyclicMethods(a.method) && !a.write))) hz += "sentinel"
    if (instance == "context" && acc.exists(_.write) && reach == "typer") hz += "context-escape"
    if (acc.exists(_.rmw)) hz += "rmw"
    if (isStamp(f) || stampStems((f.owner, stem(f.name)))) hz += "stamp-pair"
    if (f.mutableCollection.isDefined && !f.concurrentType && !f.seam && bad) hz += "collection"
    if (f.isLazy && f.forcesCompletion && Set("universe", "static", "symbol", "type", "scope", "unit", "run", "annotation", "completer")(instance)) hz += "lazy-monitor"
    val sites = accByField.getOrElse(f.key, Nil).map { a =>
      s"${a.file}:${a.line} ${if (a.mutate) "mutate" else if (a.write) "write" else "read"}${if (a.rmw) "+rmw" else ""} ${if (a.init) "init" else accessProt(a)} in ${a.method}"
    }.distinct
    Placed(f, k, kr, p, pr, ev, featuresOf(f), reach, reachRule, instance, sharedVia, hz.result(),
      acc.groupBy(accessProt).view.mapValues(_.size).toMap,
      f.typeClasses.filter(t => t != f.owner && classes.contains(t)).distinct,
      sites, unprot.map(a => s"${a.file}:${a.line} ${if (a.write) "write" else "read"} in ${a.method}").distinct,
      ks, ps, ik, ikr, ip, ipr, note, unprot.size, acc.count(_.write))
  }

  def summary: String = {
    val kinds = List("A", "B", "C", "D", "E", "L", "unclassified")
    val prots = placed.map(_.protection.takeWhile(_ != ':')).distinct.sorted
    val sb = new StringBuilder
    sb ++= s"${placed.size} fields in ${placed.map(_.f.owner).distinct.size} classes; ${accesses.size} access sites\n\n"
    sb ++= "| protection | " + kinds.mkString(" | ") + " | total |\n"
    sb ++= "|---|" + kinds.map(_ => "---:").mkString("|") + "|---:|\n"
    for (pr <- prots) {
      val row = kinds.map(k => placed.count(p => p.kind == k && p.protection.takeWhile(_ != ':') == pr))
      sb ++= s"| $pr | ${row.map(n => if (n == 0) "" else n.toString).mkString(" | ")} | ${row.sum} |\n"
    }
    val tot = kinds.map(k => placed.count(_.kind == k))
    sb ++= s"| total | ${tot.mkString(" | ")} | ${tot.sum} |\n"
    val byReach = placed.groupBy(_.reach).view.mapValues(_.size).toList.sorted
    sb ++= s"\nreach: ${byReach.map { case (r, n) => s"$r $n" }.mkString(", ")}\n"
    val wl = worklistEntries
    sb ++= s"work list: ${wl.size} fields (reach typer, protection none or partial, or a lazy-val monitor hazard)\n"
    val un = placed.filter(p => p.kind == "unclassified" && p.reach == "typer")
    sb ++= s"\nunclassified, reach typer: ${un.size}\n"
    for ((pkg, ps) <- un.groupBy(p => cls(p.f.owner).map(_.pkg).getOrElse("")).toList.sortBy(_._1))
      sb ++= s"  [$pkg] ${ps.map(p => p.f.owner.stripPrefix(pkg + ".") + "#" + p.f.name).sorted.mkString(", ")}\n"
    val annotated = placed.filter(p => p.kindSource == "annotated" || p.protectionSource == "annotated")
    sb ++= s"\nannotations.tsv: ${annotations.size} rows, ${annotated.size} override a kind or protection, the rest add a note\n"
    val unmatched = annotations.filterNot(r => placed.exists(_.f.key == r(0) + "#" + r(1)))
    for (r <- unmatched) sb ++= s"  UNMATCHED annotations.tsv row ${r(0)}#${r(1)}\n"
    sb.toString
  }

  // ---- the work list
  private val tiers = List(
    "ambient" -> ("Ambient globals", "Single-threaded-by-assumption registers of the universe (phase, run, unit, context stacks). They are the first things that must become per-thread: move into `Scratch` (or a per-unit/per-typer object passed explicitly), and make `enteringPhase`/`exitingPhase` thread-confined."),
    "shared-scratch" -> ("Scratch on a shared object", "Per-operation state kept on a per-universe singleton, so every thread uses one instance: move into `SymbolTable#Scratch`/`Frame`, or allocate per use (`ReusableInstance` in `Scratch`)."),
    "context-escape" -> ("Context state reachable from completers", "A `Context` belongs to one typer, but Namers' completers capture the context of the definition they complete and run it on whichever thread forces the symbol. These fields are written after the context is made: they need the completion lock while a completer uses the context, or the completer must work on a fresh child context."),
    "shared-singleton" -> ("Per-instance state reached through a universe singleton", "The class is per traversal or per typer, but the universe also holds one instance as an `object` (`global.typer`, an `object` TypeMap) that any thread may use: replace the singleton by a per-thread instance from `Scratch`, or allocate per call."),
    "sentinel" -> ("Sentinels and cycle detection", "The field doubles as an in-progress marker read by code that throws a cyclic-reference error: another thread reads 'in progress' as a cycle. Needs an owner-aware completion lock (the thread holding it sees a cycle, others wait), as Scala 3's `Sharing.claim`."),
    "black-hole" -> ("Black-hole protocols (lazy facts)", "Written by completion, by unpicklers and loaders, or by info transformers: complete under a completion lock, publish once the completer returns, and freeze scopes after publication (enter into a published scope only under the lock)."),
    "rmw" -> ("Non-atomic read-modify-write", "Flags, counters and appended lists updated in place: updates are lost under contention. Use an atomic (`IdCounter(isConcurrentUniverse)`, a `VarHandle` CAS for flags), or confine the writes to the completion lock."),
    "stamp-pair" -> ("Stamp and value in separate fields", "A period or run id stamp and the value it validates are written one after the other: pack them into one immutable holder in one field (Scala 3's `Stamped`), read once into a local."),
    "collection" -> ("Unsynchronised mutable collections", "A shared mutable collection: use a concurrent collection, guard it with a monitor, or make it per-thread / per-unit and merge at the end of the phase."),
    "lazy-monitor" -> ("Lazy-val monitors that force completion", "The initialiser runs under the owner's monitor and can force symbol completion: initialise eagerly before the parallel phase (e.g. in `Definitions.init`), or turn it into a publish-once value computed without holding the monitor."),
    "plain" -> ("Other unprotected fields", "No hazard shape detected beyond being written without protection: decide per field (publish-once benign race, per-thread, or synchronized).")
  )

  lazy val worklistEntries: List[(String, Placed)] = {
    val cands = placed.filter(p => p.reach == "typer" && ((p.protection == "none" || p.protection.startsWith("partial"))
      || Set("lazy-monitor", "shared-singleton", "context-escape").exists(p.hazards.contains)))
    cands.map { p =>
      val t = tiers.map(_._1).find(t => t != "plain" && p.hazards.contains(t)).getOrElse("plain")
      (t, p)
    }
  }

  private def discipline(t: String, p: Placed): String = {
    if (p.note.nonEmpty) p.note
    else (t, p.kindRule) match {
      case ("ambient", _) => "per-thread (Scratch) or passed explicitly"
      case ("shared-scratch", _) | ("shared-singleton", _) => "per-thread via Scratch"
      case ("context-escape", _) => "completion lock while a completer uses the context, or a fresh child context"
      case (_, "E.settings") => "written at startup: freeze before the parallel phase (init-only)"
      case (_, "B.symbol") => "completion lock; publish after completion"
      case (_, "B.scope") => "freeze after completion; enter under the completion lock"
      case (_, "B.completer") | (_, "B.unpickler") => "completion lock (the completer's)"
      case ("sentinel", _) => "owner-aware completion lock instead of a sentinel"
      case ("rmw", _) if p.kind == "D" => "atomic (IdCounter / AtomicInteger)"
      case ("rmw", _) => "atomic update, or only under the completion lock"
      case ("stamp-pair", _) => "one immutable (stamp, value) holder in one field"
      case ("collection", "C.per-run") => "concurrent map, or per-thread and merged"
      case ("collection", _) if p.kind == "E" => "synchronized, or per-unit and merged"
      case ("collection", _) => "concurrent collection or synchronized"
      case ("lazy-monitor", _) => "initialise eagerly before typer, or publish-once without the monitor"
      case (_, k) if k.startsWith("C.") => "benign race if idempotent and read once into a local; else a holder"
      case (_, "E.tree") => "confined to the unit's thread (trees belong to a unit)"
      case (_, "E.unit") => "confined to the unit's thread"
      case (_, "E.run") => "synchronized, or per-unit and merged"
      case (_, "D.counter") => "atomic (IdCounter)"
      case _ => "decide: per-thread, publish-once benign, or synchronized"
    }
  }

  def worklist(tip: String, gh: String): String = {
    val entries = worklistEntries
    val sb = new StringBuilder
    sb ++= "# Work list: shared mutable state reachable from the typer\n\n"
    sb ++= s"Generated by `tools/state-map/StateMap.scala` at `${tip.take(10)}`; do not edit by hand (overrides go in `annotations.tsv`).\n\n"
    sb ++= s"The fields below have reach `typer` (owned by code that runs in parser → namer → packageobjects → typer → superaccessors → extmethods → pickler) and no protection at some access (`none` or `partial:…`), plus lazy vals whose initialiser can force symbol completion and per-instance scratch reachable through a universe singleton. ${entries.size} fields in ${entries.map(_._2.f.owner).distinct.size} owners. Each field appears once, under its most severe hazard. Within a section, owners are ordered by their number of unprotected access sites.\n\n"
    sb ++= "Line format: `field: Type` — `file:line` — kind/rule — instance — unprotected sites (writes) — hazards — suggested discipline.\n\n"
    sb ++= "| section | fields |\n|---|---:|\n"
    for ((t, (title, _)) <- tiers) { val n = entries.count(_._1 == t); if (n > 0) sb ++= s"| [$title](#${title.toLowerCase.replaceAll("[^a-z0-9 -]", "").replace(' ', '-')}) | $n |\n" }
    sb ++= "\n"
    for ((t, (title, why)) <- tiers) {
      val es = entries.filter(_._1 == t).map(_._2)
      if (es.nonEmpty) {
        sb ++= s"## $title\n\n$why\n\n"
        val byOwner = es.groupBy(_.f.owner).toList.sortBy { case (o, ps) => (-ps.map(_.unprotectedCount).sum, o) }
        for ((o, ps) <- byOwner) {
          val inst = ps.head.instance
          val via = if (ps.head.sharedVia.nonEmpty && ps.head.kind == "A") s", also as ${ps.head.sharedVia.map(_.split('.').last).mkString(", ")}" else ""
          sb ++= s"### `$o` ($inst$via)\n\n"
          if (t == "lazy-monitor" && ps.size > 12) {
            // many lazy vals of one owner share one monitor and one discipline: one line, linked by line number
            val f0 = ps.head.f
            sb ++= s"${ps.size} lazy vals in [`${f0.file}`]($gh/$tip/${f0.file}), all under the monitor of one instance; ${discipline(t, ps.head)}:\n"
            sb ++= ps.sortBy(_.f.line).map(p => s"[`${p.f.name}`]($gh/$tip/${p.f.file}#L${p.f.line}) L${p.f.line}").mkString("", ", ", "\n\n")
          } else
          for (p <- ps.sortBy(p => (-p.unprotectedCount, p.f.name))) {
            val f = p.f
            val decl = if (f.isLazy) "lazy val" else if (f.isVar) "var" else "val"
            val calls = if (f.isLazy && f.lazyCalls.nonEmpty) s" — calls ${f.lazyCalls.take(5).mkString(", ")}" else ""
            sb ++= s"- `${f.name}: ${f.tpe.take(60)}` ($decl) — [`${f.file}:${f.line}`]($gh/$tip/${f.file}#L${f.line}) — ${p.kind}/${p.kindRule.stripPrefix(p.kind + ".")}${if (p.protection != "none") " — " + p.protection else ""} — ${p.unprotectedCount} sites (${p.writeCount} w) — ${p.hazards.mkString(", ")}$calls — ${discipline(t, p)}\n"
          }
          sb ++= "\n"
        }
      }
    }
    sb.toString
  }

  def toJson(tip: String, gh: String, sitesCap: Int = Int.MaxValue): String = {
    val ruleJson = (Rules.kind ++ Rules.protection ++ Rules.reach ++ Rules.excluded).map(r => jobj("id" -> jstr(r.id), "sets" -> jstr(r.sets), "doc" -> jstr(r.doc)))
    val featJson = Rules.feature.map { case (k, d) => jobj("id" -> jstr(k), "doc" -> jstr(d)) }
    val hazJson = Rules.hazard.map { case (k, d) => jobj("id" -> jstr(k), "doc" -> jstr(d)) }
    val fieldsJson = placed.map { p =>
      val f = p.f
      jobj(
        "key" -> jstr(f.key), "owner" -> jstr(f.owner), "name" -> jstr(f.name), "type" -> jstr(f.tpe),
        "file" -> jstr(f.file), "line" -> f.line.toString, "url" -> jstr(s"$gh/$tip/${f.file}#L${f.line}"),
        "annotations" -> jarr(if (f.isVolatile) List(jstr("volatile")) else Nil), "var" -> f.isVar.toString, "lazy" -> f.isLazy.toString,
        "private" -> f.isPrivate.toString,
        "mutableCollection" -> f.mutableCollection.map(jstr).getOrElse("null"),
        "kind" -> jstr(p.kind), "kindRule" -> jstr(p.kindRule),
        "protection" -> jstr(p.protection), "protectionRule" -> jstr(p.protRule), "evidence" -> jstr(p.evidence),
        "features" -> jarr(p.features.map(jstr)), "role" -> jstr(if (p.kind == "C") "cache" else "semantic"), "reach" -> jstr(p.reach),
        "reachRule" -> jstr(p.reachRule), "instance" -> jstr(p.instance), "sharedVia" -> jarr(p.sharedVia.map(jstr)),
        "hazards" -> jarr(p.hazards.map(jstr)),
        "forcesCompletion" -> f.forcesCompletion.toString, "lazyCalls" -> jarr(f.lazyCalls.map(jstr)),
        "accesses" -> jobj(p.accessSummary.toList.sorted.map { case (k, v) => k -> v.toString }: _*),
        "edges" -> jarr(p.edges.map(jstr)),
        "sites" -> jarr(p.sites.take(sitesCap).map(jstr)), "siteCount" -> p.sites.size.toString,
        "unprotectedSites" -> jarr(p.unprotectedSites.take(sitesCap).map(jstr)),
        "kindSource" -> jstr(p.kindSource), "protectionSource" -> jstr(p.protectionSource),
        "inferredKind" -> jstr(p.inferredKind), "inferredKindRule" -> jstr(p.inferredKindRule),
        "inferredProtection" -> jstr(p.inferredProtection), "inferredProtectionRule" -> jstr(p.inferredProtectionRule),
        "note" -> jstr(p.note))
    }
    val owners = placed.map(_.f.owner).toSet ++ placed.flatMap(_.edges)
    val classesJson = classes.values.filter(ci => owners(ci.name)).toList.sortBy(_.name).map { ci =>
      jobj("name" -> jstr(ci.name), "simple" -> jstr(ci.simple), "pkg" -> jstr(ci.pkg), "file" -> jstr(ci.file),
        "line" -> ci.line.toString, "url" -> jstr(s"$gh/$tip/${ci.file}#L${ci.line}"), "module" -> ci.isModule.toString,
        "trait" -> ci.isTrait.toString, "instance" -> jstr(ci.instance),
        "bases" -> jarr(ci.bases.filter(b => owners(b) && b != ci.name).map(jstr)))
    }
    jtop(
      "header" -> jobj(
        "generator" -> jstr("tools/state-map/StateMap.scala"),
        "tip" -> jstr(tip), "github" -> jstr(gh),
        "kinds" -> jobj(
          "A" -> jstr("per-operation scratch"), "B" -> jstr("lazy facts with a black hole"),
          "C" -> jstr("caches of derived values"), "D" -> jstr("interning and counters"), "E" -> jstr("trees, units, run and universe state"),
          "L" -> jstr("lazy vals (monitor on init)"), "unclassified" -> jstr("no rule places it")),
        "rules" -> ruleJson.mkString("[\n", ",\n", "\n]"),
        "features" -> featJson.mkString("[\n", ",\n", "\n]"),
        "hazards" -> hazJson.mkString("[\n", ",\n", "\n]"),
        "note" -> jstr("Rules are tried in order; the first that matches sets kind and protection. `evidence` says what the protection rests on: lexical (the accesses), annotation, type, owner, or declaration.")),
      "classes" -> classesJson.mkString("[\n", ",\n", "\n]"),
      "fields" -> fieldsJson.mkString("[\n", ",\n", "\n]"))
  }
}
