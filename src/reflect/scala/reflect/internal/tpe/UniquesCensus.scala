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
package tpe

import java.io.{File, PrintWriter}

import scala.collection.mutable
import util.ChunkedInterner

/** DO NOT MERGE. The uniques census, `-Dscala.uniques.census=true` with `-Dscala.uniques.layout=chunked-weak`.
 *
 *  For each type the table interns, it records the kind of type, the phase, whether it was made inside a completion
 *  (a `CompletionFrame` deeper than the outermost), and whether its shape refers to a method's params (as in
 *  retronym/scala3#112) or to skolems or existentially bound symbols. It then follows each type to its fate:
 *
 *  - `anchoredSame`: became reachable from a symbol's info (or a symbol's type cache) in the same entry into a frame
 *    that made it, so a mark at construction could have caught it;
 *  - `anchoredLater`: became reachable from a symbol's info later;
 *  - `anchoredCleared`: became reachable from a symbol's info, and was collected all the same (its symbol died);
 *  - `clearedFree`: never anchored, collected within the run;
 *  - `liveFree`: never anchored, still alive when the run ended (held by trees, caches or other types).
 *
 *  A symbol's info is walked when it is set (`info_=`, `updateInfo`, an info transformer) and when a symbol caches its
 *  `ThisType` or its type constructor, without forcing anything. The walk stops at a type that is already anchored.
 *
 *  Counters accumulate over the whole JVM and are written in perflab-agent's report format to
 *  `<perflab.cell>/audit-<pid>.json` at the end of each run (or `scala.uniques.census.out`), so that `perflab api audit`
 *  reads them.
 */
trait UniquesCensus { self: SymbolTable =>
  import UniquesCensus._

  final val uniquesCensusOn: Boolean = isCompilerUniverse && java.lang.Boolean.getBoolean("scala.uniques.census")
  /** `-Dscala.uniques.census.sites=true`: also record, per anchor and per intern, the code that did it (slow). */
  private[this] val censusSites: Boolean = java.lang.Boolean.getBoolean("scala.uniques.census.sites")
  /** `-Dscala.uniques.census.provenance=<n>`: for 1 in n types interned outside closed scopes, record which kind of
   *  operation made it (implicit search, silent typing, inference, namer, typer, a transform...) as its site. */
  private[this] val censusGcAtEnd: Boolean = java.lang.Boolean.getBoolean("scala.uniques.census.gcAtEnd")
  private[this] val censusProvenance: Int = Integer.getInteger("scala.uniques.census.provenance", 0).intValue
  /** `-Dscala.uniques.census.promote=true`: the rule promotes a weak type that a lookup outside every scope finds.
   *  Off by default: the hits are counted (`rule.hitsFromStrong`) but the type stays weak. */
  private[this] val censusPromote: Boolean = java.lang.Boolean.getBoolean("scala.uniques.census.promote")

  private[this] lazy val census = new Census

  /** DO NOT MERGE: `-Dscala.uniques.uncache=query,narrow,...|all`: the weak scopes whose new types are not interned. */
  /** The weak scopes whose new types are not interned, from `uniquesUncache` (`-Yuniques-uncache`, else
   *  `-Dscala.uniques.uncache`), read when each run creates its table. */
  final var uncacheMask: Int = 0
  private[internal] final def refreshUncache(): Unit = {
    val spec = if (isCompilerUniverse) uniquesUncache else ""
    val labels = spec.split("[,+]").map(_.trim).filter(_.nonEmpty) // + as well: JMH splits a -p value at commas
    uncacheMask =
      if (labels.contains("all")) ~1 & ~StrongCandidateLabels
      else if (labels.contains("closed")) ClosedWeakLabels
      else labels.foldLeft(0) { (m, l) =>
        val i = WeakNames.indexOf(l)
        require(i > 0, s"-Yuniques-uncache: unknown scope $l, expected one of ${WeakNames.drop(1).mkString(", ")}")
        m | (1 << i)
      }
    weakScopesOn = uniquesCensusOn || uncacheMask != 0
  }
  /** `-Dscala.uniques.uncache.cache=<slots>`: the direct-mapped cache of uncached types, a power of two, 0 for none. */
  final val uncacheCacheSize: Int = {
    val n = Integer.getInteger("scala.uniques.uncache.cache", 4096).intValue
    if (n <= 0) 0 else Integer.highestOneBit(n)
  }
  private[this] var weakScopesOn: Boolean = uniquesCensusOn

  private[internal] final def uncacheCount(what: Int): Unit = if (uniquesCensusOn) census.uncacheCount(what)
  private[internal] final def uncacheMiss(tp: Type): Unit = if (uniquesCensusOn) census.uncacheMiss(tp)

  /** Enter a scope whose interned types the rule under test would hold weakly, at this completion level only: a
   *  completion started inside it runs in a fresh frame, outside any weak scope. Returns null when the census is off. */
  final def weakScopeEnter(label: Int): CompletionFrame =
    if (!weakScopesOn) null
    else {
      val f = scratch.frame
      if (f.weakDepth == 0) f.weakLabel = label
      f.weakDepth += 1
      f
    }
  /** DO NOT MERGE: leave the current weak scope for a computation whose result outlives it (a symbol's info transform,
   *  a symbol's type caches): it is interned as outside any scope. Returns what `weakScopeResume` restores. Weak scopes
   *  entered inside stay unlabelled, as the enclosing depth is still positive. */
  final def weakScopeSuspend(): Int =
    if (!weakScopesOn) 0
    else { val f = scratch.frame; val l = f.weakLabel; f.weakLabel = 0; l }
  final def weakScopeResume(label: Int): Unit = if (label != 0) scratch.frame.weakLabel = label

  /** DO NOT MERGE: run in strong-scope candidate `label` (a census label): returns what `strongScopeExit` restores. */
  final def strongScopeEnter(label: Int): Int =
    if (!weakScopesOn) -1
    else { val f = scratch.frame; val old = f.strongLabel; f.strongLabel = label; old }
  final def strongScopeExit(old: Int): Unit = if (old != -1) scratch.frame.strongLabel = old

  final def weakScopeExit(f: CompletionFrame): Unit =
    if (f ne null) {
      f.weakDepth -= 1
      if (f.weakDepth == 0) f.weakLabel = 0
    }

  /** Called by the table each run, when it is created. */
  private[internal] def uniquesCensusAttach(table: ChunkedInterner[Type]): Unit =
    if (uniquesCensusOn) census.attach(table)

  /** `tp` is now reachable from a symbol's info or one of its caches, by way `src`. */
  final def uniquesCensusAnchor(sym: Symbol, tp: Type, src: Int): Unit =
    if (uniquesCensusOn) census.anchor(sym, tp, src)

  final def uniquesCensusReport(): Unit =
    if (uniquesCensusOn) census.report()

  /** The types `tp` is made of, without forcing anything. The infos of params and quantified symbols are left out:
   *  they are walked when they are set. */
  private def foreachPart(tp: Type, f: Type => Unit): Unit = tp match {
    case TypeRef(pre, _, args)         => f(pre); args.foreach(f)
    case SingleType(pre, _)            => f(pre)
    case SuperType(thistp, supertp)    => f(thistp); f(supertp)
    case TypeBounds(lo, hi)            => f(lo); f(hi)
    case RefinedType(parents, _)       => parents.foreach(f)
    case ClassInfoType(parents, _, _)  => parents.foreach(f)
    case MethodType(_, res)            => f(res)
    case PolyType(_, res)              => f(res)
    case NullaryMethodType(res)        => f(res)
    case ExistentialType(_, underlying) => f(underlying)
    case AnnotatedType(_, underlying)  => f(underlying)
    case ErasedValueType(_, underlying) => f(underlying)
    case _                             =>
  }

  private final class Census extends ChunkedInterner.Listener {
    private[this] var table: ChunkedInterner[Type] = null
    private[this] val classIds = new mutable.HashMap[Class[_], Int]
    private[this] val classNames = new Array[String](MaxClasses)

    // [cls][ctx][shape]
    private[this] val addedCls = new Array[Long](MaxClasses * 2 * 4)
    // [phase][ctx][shape]
    private[this] val addedPhase = new Array[Long](MaxPhases * 2 * 4)
    // [cls][ctx][shape][fate][src]
    private[this] val fateCls = new Array[Long](MaxClasses * 2 * 4 * Fates * Srcs)
    // [phase][ctx][shape][fate]
    private[this] val fatePhase = new Array[Long](MaxPhases * 2 * 4 * Fates)
    // sites: id -> key, and the fates of the types each site anchored or created
    private[this] val siteIds = new mutable.HashMap[String, Int]
    private[this] val siteKeys = mutable.ArrayBuffer[String]("-")
    private[this] val anchorSiteFates = new mutable.HashMap[Int, Array[Long]]
    private[this] val createSiteFates = new mutable.HashMap[Int, Array[Long]]
    private[this] val addedScope = new Array[Long](WeakLabels)
    // [label][promoted][fate]
    private[this] val fateScope = new Array[Long](WeakLabels * 2 * Fates)
    private[this] var promotions = 0L
    // uncached lookups: found in the table, found in the direct-mapped cache, neither (a new instance); of the new
    // instances, how many equal one made earlier in the run that is still alive (sharing the table would have kept)
    private[this] val uncacheCounts = new Array[Long](3)
    private[this] var lostSharing = 0L
    private[this] var shadow: util.WeakHashSet[Type] = null
    def uncacheCount(what: Int): Unit = uncacheCounts(what) += 1
    def uncacheMiss(tp: Type): Unit = {
      uncacheCounts(UncacheMiss) += 1
      if (shadow == null) shadow = util.WeakHashSet[Type](4096)
      if (shadow.findEntryOrUpdate(tp) ne tp) lostSharing += 1
    }
    private[this] var anchorWalks = 0L
    private[this] var anchorVisits = 0L
    private[this] var runs = 0

    def attach(t: ChunkedInterner[Type]): Unit = {
      if (table ne null) finish()
      table = t
      if (t.weak) t.listener = this
      else table = null
    }

    private def classId(c: Class[_]): Int = classIds.getOrElseUpdate(c, {
      val id = math.min(classIds.size, MaxClasses - 1)
      if (classNames(id) == null) classNames(id) = if (id == MaxClasses - 1) "other" else c.getName.stripPrefix("scala.reflect.internal.Types$")
      id
    })

    def added(ref: ChunkedInterner.Ref): Unit = {
      val tp = ref.get().asInstanceOf[Type]
      val frame = scratch.frame
      val ctx = if (frame.depth > 0) 1 else 0
      val cls = classId(tp.getClass)
      val ph = math.min(phase.id, MaxPhases - 1)
      val sh = shapeOf(tp)
      // a closed scope's label wins; otherwise the strong-scope candidate the type is made in, if any
      val wl = if (isClosedWeakLabel(frame.weakLabel) || frame.strongLabel == 0) frame.weakLabel else frame.strongLabel
      ref.census = cls | (ph << PhaseShift) | (ctx << CtxShift) | (sh << ShapeShift) | (wl << WeakShift)
      addedScope(wl) += 1
      ref.serial = frame.serial
      if (censusSites) ref.site = siteId(stackKey(CreateSkip, 6))
      else if (censusProvenance > 0 && !isClosedWeakLabel(wl)) {
        provenanceCount += 1
        if (provenanceCount % censusProvenance == 0) ref.site = siteId("prov:" + provenance())
      }
      addedCls((cls * 2 + ctx) * 4 + sh) += 1
      addedPhase((ph * 2 + ctx) * 4 + sh) += 1
    }

    /** A lookup outside any weak scope found a type made in one: the rule would make it strong. */
    def hit(ref: ChunkedInterner.Ref): Unit = {
      val c = ref.census
      if (((c >>> WeakShift) & WeakMask) != 0 && (c & Promoted) == 0 && scratch.frame.weakLabel == 0) {
        ref.census = c | Promoted
        promotions += 1 // types first found from outside every scope
      }
    }

    def cleared(ref: ChunkedInterner.Ref): Unit = {
      val c = ref.census
      if ((c & Counted) == 0) {
        ref.census = c | Counted
        val fate = if ((c & Anchored) != 0) AnchoredCleared else ClearedFree
        count(c, fate)
        countSite(ref, fate)
      }
    }

    private def count(c: Int, fate: Int): Unit = {
      val cls = c & ClassMask
      val ph = (c >>> PhaseShift) & PhaseMask
      val ctx = (c >>> CtxShift) & 1
      val sh = (c >>> ShapeShift) & 3
      val src = (c >>> SrcShift) & SrcMask
      fateCls((((cls * 2 + ctx) * 4 + sh) * Fates + fate) * Srcs + src) += 1
      fatePhase(((ph * 2 + ctx) * 4 + sh) * Fates + fate) += 1
      val wl = (c >>> WeakShift) & WeakMask
      fateScope((wl * 2 + (if ((c & Promoted) != 0) 1 else 0)) * Fates + fate) += 1
    }

    private[this] var provenanceCount = 0L
    /** The kind of operation on the stack, by the first of these found anywhere on it. */
    private def provenance(): String = if (provenanceInnermost) innermostProvenance() else {
      val st = new Throwable().getStackTrace
      def has(p: String): Boolean = { var i = 0; while (i < st.length) { val e = st(i); if ((e.getClassName + "." + e.getMethodName).startsWith(p)) return true; i += 1 }; false }
      def hasClass(p: String): Boolean = { var i = 0; while (i < st.length) { if (st(i).getClassName.startsWith(p)) return true; i += 1 }; false }
      if (hasClass("scala.tools.nsc.typechecker.Implicits$ImplicitSearch")) "implicitSearch"
      else if (has("scala.tools.nsc.typechecker.Typers$Typer.silent")) "silent"
      else if (hasClass("scala.tools.nsc.typechecker.Infer$Inferencer")) "infer"
      else if (hasClass("scala.reflect.internal.pickling.UnPickler") || hasClass("scala.tools.nsc.symtab.classfile.ClassfileParser")) "load"
      else if (hasClass("scala.tools.nsc.typechecker.Namers")) "namer"
      else if (has("scala.tools.nsc.typechecker.Typers$Typer.adapt")) "typer.adapt"
      else if (hasClass("scala.tools.nsc.typechecker.Typers")) "typer"
      else if (hasClass("scala.tools.nsc.typechecker.RefChecks")) "refchecks"
      else {
        var i = 0; var r: String = null
        while ((r eq null) && i < st.length) {
          val c = st(i).getClassName
          if (c.startsWith("scala.tools.nsc.transform.") || c.startsWith("scala.tools.nsc.backend.")) r = c.split('.')(4).takeWhile(_ != '$')
          i += 1
        }
        if (r ne null) "phase:" + r else "other"
      }
    }

    /** `-Dscala.uniques.census.provenance.innermost=true`: classify by the innermost marker on the stack instead. */
    private[this] val provenanceInnermost = java.lang.Boolean.getBoolean("scala.uniques.census.provenance.innermost")
    private def innermostProvenance(): String = {
      val st = new Throwable().getStackTrace
      var i = 0
      while (i < st.length) {
        val c = st(i).getClassName; val m = st(i).getMethodName
        // A completion runs in a CompletionFrame of its own: what forced it does not classify its types. Without a
        // marker inside the completion, the type is the completer's.
        if (c == "scala.reflect.internal.Symbols$Symbol" && m == "completeInfo") {
          var j = i - 1
          while (j >= 0 && st(j).getMethodName != "complete") j -= 1
          val completer = if (j >= 0) st(j).getClassName else "?"
          return "completion:" + completer.substring(completer.lastIndexOf('.') + 1).replaceAll("\\$\\$anon\\$\\d+|\\$\\d+", "")
        }
        val r =
          if (c.startsWith("scala.tools.nsc.typechecker.Implicits$ImplicitSearch")) "implicitSearch"
          else if (c == "scala.tools.nsc.typechecker.Typers$Typer" && m == "silent") "silent"
          else if (c.startsWith("scala.tools.nsc.typechecker.Infer$Inferencer")) "infer"
          else if (c.startsWith("scala.reflect.internal.pickling.UnPickler") || c.startsWith("scala.tools.nsc.symtab.classfile.ClassfileParser")) "load"
          else if (c.startsWith("scala.tools.nsc.typechecker.Namers")) "namer"
          else if (c == "scala.tools.nsc.typechecker.Typers$Typer" && (m == "adapt" || m.startsWith("adapt"))) "typer.adapt"
          else if (c.startsWith("scala.tools.nsc.typechecker.RefChecks")) "refchecks"
          else if (c.startsWith("scala.tools.nsc.typechecker.Typers")) "typer"
          else if (c.startsWith("scala.tools.nsc.transform.") || c.startsWith("scala.tools.nsc.backend.")) "phase:" + c.split('.')(4).takeWhile(_ != '$')
          else null
        if (r ne null) return r
        i += 1
      }
      "other"
    }

    private def countSite(r: ChunkedInterner.Ref, fate: Int): Unit = if (censusSites || (censusProvenance > 0 && r.site != 0)) {
      val verdict = scopeVerdict(r.census)
      createSiteFates.getOrElseUpdate(r.site, new Array[Long](Fates * 3))(verdict * Fates + fate) += 1
      val as = (r.census >>> AnchorSiteShift) & AnchorSiteMask
      if (as != 0) anchorSiteFates.getOrElseUpdate(anchorSiteIds(as), new Array[Long](Fates))(fate) += 1
    }
    // anchor sites get an index of their own, small enough to fit the census bits
    private[this] val anchorSiteIds = mutable.ArrayBuffer[Int](0)
    private[this] val anchorSiteIndex = new mutable.HashMap[Int, Int]
    private def anchorSiteIndexOf(site: Int): Int = anchorSiteIndex.getOrElseUpdate(site, {
      if (anchorSiteIds.size > AnchorSiteMask) AnchorSiteMask else { anchorSiteIds += site; anchorSiteIds.size - 1 }
    })

    /** 0: the rule holds it strongly (made outside every weak scope); 1: weakly; 2: made weak, then promoted. */
    private def scopeVerdict(c: Int): Int =
      if (((c >>> WeakShift) & WeakMask) == 0) 0 else if (censusPromote && (c & Promoted) != 0) 2 else 1

    private def siteId(key: String): Int = siteIds.getOrElseUpdate(key, { siteKeys += key; siteKeys.size - 1 })

    private def stackKey(skip: java.util.regex.Pattern, depth: Int): String = {
      val st = new Throwable().getStackTrace
      val sb = new java.lang.StringBuilder
      var n = 0
      var i = 0
      while (i < st.length && n < depth) {
        val f = st(i)
        val m = f.getClassName + "." + f.getMethodName
        if (!skip.matcher(m).find()) {
          if (n > 0) sb.append(" <- ")
          sb.append(f.getClassName.stripPrefix("scala.reflect.internal.").stripPrefix("scala.tools.nsc.")).append('.').append(f.getMethodName).append(':').append(f.getLineNumber)
          n += 1
        }
        i += 1
      }
      sb.toString
    }

    private def symKind(sym: Symbol): String = {
      val what =
        if (sym.isTypeSkolem) "type skolem"
        else if (sym.isExistentiallyBound) "existential"
        else if (sym.isTypeParameter) "type param"
        else if (sym.isValueParameter) "value param"
        else if (sym.isAbstractType) "abstract type"
        else if (sym.isAliasType) "alias type"
        else if (sym.isClass) "class"
        else if (sym.isModule) "module"
        else if (sym.isMethod) "method"
        else if (sym.isTerm) "term"
        else "type"
      val owner = sym.owner
      val of =
        if (owner.isMethod) "method"
        else if (owner.isClass) (if (owner.isRefinementClass) "refinement" else "class")
        else if (owner.isTerm) "term"
        else if (owner.isAbstractType || owner.isAliasType) "type"
        else "other"
      s"$what of $of"
    }

    /** The shape of `tp`: bit 0, it refers to a method's (type) param; bit 1, to a skolem or existentially bound
     *  symbol. Taken from the census bits of its interned parts, else by walking its structure. */
    private def shapeOf(tp: Type): Int = {
      var sh = 0
      def leaf(t: Type): Unit = t match {
        case TypeRef(_, sym, _) if sym.isAbstractType =>
          if (sym.isSkolem || sym.isExistentiallyBound) sh |= 2
          else if (sym.isTypeParameter && sym.owner.isMethod) sh |= 1
        case SingleType(NoPrefix, sym) =>
          if (sym.owner.isMethod) sh |= 1
        case _ =>
      }
      def part(t: Type): Unit = t match {
        case u: UniqueType if table ne null =>
          val r = table.refOf(u, u.hashCode)
          if (r ne null) sh |= (r.census >>> ShapeShift) & 3
          else { leaf(u); foreachPart(u, part) }
        case _ => leaf(t); foreachPart(t, part)
      }
      leaf(tp)
      foreachPart(tp, part)
      sh
    }

    def anchor(sym: Symbol, tp: Type, src0: Int): Unit = if (table ne null) {
      val src =
        if (src0 != SrcInfo) src0
        else if (sym.isValueParameter || (sym.isTypeParameterOrSkolem && !sym.owner.isClass)) SrcParam
        else if (sym.isLocalToBlock) SrcLocal
        else if (sym.owner.isClass) SrcMember
        else SrcOther
      val serial = scratch.frame.serial
      anchorWalks += 1
      var site = 0 // computed on the first new anchor of this walk
      def siteBits: Int = if (!censusSites) 0 else {
        if (site == 0) site = anchorSiteIndexOf(siteId(SrcNames(src) + " | " + symKind(sym) + " | " + stackKey(AnchorSkip, 6)))
        site << AnchorSiteShift
      }
      def visit(t: Type): Unit = {
        anchorVisits += 1
        t match {
          case u: UniqueType =>
            val r = table.refOf(u, u.hashCode)
            if (r ne null) {
              val c = r.census
              if ((c & Anchored) == 0) {
                r.census = c | Anchored | (if (r.serial == serial) SameFrame else 0) | (src << SrcShift) | siteBits
                foreachPart(u, visit)
              }
            } else foreachPart(u, visit)
          case _ => foreachPart(t, visit)
        }
      }
      visit(tp)
    }

    /** At the end of a run, every type still in the table meets its fate. */
    private def finish(): Unit = if (table ne null) {
      // -Dscala.uniques.census.gcAtEnd=true: collect first, so that a type no longer reachable at the end of the run
      // counts as cleared, not as alive because no collection has run since it died
      if (censusGcAtEnd) { System.gc(); System.gc() }
      table.foreachRef { r =>
        val c = r.census
        if ((c & Counted) == 0) {
          r.census = c | Counted
          val fate =
            if (r.get() == null) { if ((c & Anchored) != 0) AnchoredCleared else ClearedFree }
            else if ((c & Anchored) == 0) LiveFree
            else if ((c & SameFrame) != 0) AnchoredSame
            else AnchoredLater
          count(c, fate)
          countSite(r, fate)
        }
      }
      table.listener = null
      table = null
      shadow = null
      runs += 1
    }

    private[this] var lastCounters: collection.Map[String, Long] = Map.empty
    private[this] var lastProbes: collection.Map[(String, String), Long] = Map.empty

    def report(): Unit = {
      finish()
      val counters = new mutable.TreeMap[String, Long]
      def add(k: String, n: Long): Unit = if (n != 0) counters(k) = counters.getOrElse(k, 0L) + n
      add("runs", runs)
      add("anchor.walks", anchorWalks)
      add("anchor.visits", anchorVisits)
      add("rule.hitsFromStrong", promotions)
      add("uncache.tableHit", uncacheCounts(UncacheTableHit))
      add("uncache.cacheHit", uncacheCounts(UncacheCacheHit))
      add("uncache.miss", uncacheCounts(UncacheMiss))
      add("uncache.lostSharing", lostSharing)
      for (wl <- 0 until WeakLabels) {
        add(s"rule.added.${WeakNames(wl)}", addedScope(wl))
        for (pr <- 0 to 1; f <- 0 until Fates) {
          val n = fateScope((wl * 2 + pr) * Fates + f)
          val verdict = if (wl == 0) "strong" else if (pr == 1 && censusPromote) "promoted" else "weak"
          add(s"rule.$verdict.${FateNames(f)}", n)
          if (wl != 0 && pr == 1) add(s"rule.hitFromStrong.${FateNames(f)}", n)
          if (wl != 0) add(s"rule.scope.${WeakNames(wl)}${if (pr == 1) ".promoted" else ""}.${FateNames(f)}", n)
        }
      }
      for (cls <- 0 until MaxClasses; ctx <- 0 to 1; sh <- 0 to 3) {
        val n = addedCls((cls * 2 + ctx) * 4 + sh)
        add("added", n)
        add(s"added.ctx.${CtxNames(ctx)}", n)
        add(s"added.shape.${ShapeNames(sh)}", n)
        if (n != 0) add(s"added.cls.${classNames(cls)}", n)
        for (f <- 0 until Fates; src <- 0 until Srcs) {
          val m = fateCls((((cls * 2 + ctx) * 4 + sh) * Fates + f) * Srcs + src)
          val fn = FateNames(f)
          add(s"fate.$fn", m)
          add(s"fate.$fn.ctx.${CtxNames(ctx)}", m)
          add(s"fate.$fn.shape.${ShapeNames(sh)}", m)
          add(s"fate.$fn.ctx.${CtxNames(ctx)}.shape.${ShapeNames(sh)}", m)
          if (m != 0) add(s"fate.$fn.cls.${classNames(cls)}", m)
          if (f >= AnchoredSame) add(s"fate.$fn.src.${SrcNames(src)}", m)
        }
      }
      for (ph <- 0 until MaxPhases) {
        val name = if (ph < phaseWithId.length && phaseWithId(ph) != null) phaseWithId(ph).name else s"phase$ph"
        for (ctx <- 0 to 1; sh <- 0 to 3) {
          add(s"added.phase.$name", addedPhase((ph * 2 + ctx) * 4 + sh))
          for (f <- 0 until Fates) add(s"fate.${FateNames(f)}.phase.$name", fatePhase(((ph * 2 + ctx) * 4 + sh) * Fates + f))
        }
      }
      // This census's own probes, every site
      val probes = new mutable.HashMap[(String, String), Long]
      for {
        (label, m, verdicts) <- List(("anchor", anchorSiteFates, List("")), ("create", createSiteFates, List("strong.", "weak.", "promoted.")))
        (vname, v) <- verdicts.zipWithIndex
        f <- 0 until Fates
        (id, a) <- m
        n = a(v * Fates + f)
        if n > 0
      } probes((label + "." + vname + FateNames(f), siteKeys(id))) = n
      // Each op of a benchmark compiles with a new Global, and so a new census: merge what this one counted since its
      // last report into the totals for the JVM, which the report file holds.
      val totals = UniquesCensus.merge(counters, lastCounters, probes, lastProbes)
      lastCounters = counters
      lastProbes = probes
      val out = UniquesCensus.write(totals)
      def c(k: String) = counters.getOrElse(k, 0L)
      def r(v: String) = FateNames.map(f => s"$f=${c(s"rule.$v.$f")}").mkString(" ")
      System.out.println(s"[uniques-census] rule: strong{${r("strong")}} weak{${r("weak")}} promoted{${r("promoted")}} hitFromStrong{${r("hitFromStrong")}}")
      if (uncacheMask != 0) System.out.println(s"[uniques-census] uncache: tableHit=${c("uncache.tableHit")} cacheHit=${c("uncache.cacheHit")} miss=${c("uncache.miss")} lostSharing=${c("uncache.lostSharing")}")
      System.out.println(s"[uniques-census] this Global: runs=$runs added=${c("added")} anchoredSame=${c("fate.anchoredSame")} anchoredLater=${c("fate.anchoredLater")} anchoredCleared=${c("fate.anchoredCleared")} clearedFree=${c("fate.clearedFree")} liveFree=${c("fate.liveFree")} -> $out")
    }
  }

  // ---- DO NOT MERGE: escape check for -Yuniques-uncache ----------------------------------------------------------
  //
  // The weak scopes fall in two categories. Closed (`ClosedWeakLabels`, the boolean queries) return a Boolean, so a
  // type they make should be garbage when they return; open ones (type maps that clone params, instantiation,
  // existential packing, mixin's clones, adapt's instantiation, stableTypeFor, narrowing) make the types they return.
  // `-Dscala.uniques.escapeCheck=true` looks for types returned uninterned by a closed scope (`uniquesEscapeTag`) in
  // the pickles, and in a walk of the run's trees and the symbol table (every info in each symbol's history, through
  // the parts of types, params, type params, quantified symbols, refinement and class members, owners and annotations).

  final val uniquesEscapeCheckOn: Boolean = isCompilerUniverse && java.lang.Boolean.getBoolean("scala.uniques.escapeCheck")
  private[this] lazy val escapeCheck = new EscapeCheck

  /** `tp` is about to be written to sink `sink` (e.g. a pickle). */
  final def uniquesEscapeNote(sink: String, tp: Type): Unit =
    if (uniquesEscapeCheckOn && tp.uniquesEscapeTag != 0) escapeCheck.note(sink, tp, "")

  /** Walk `trees` and the symbol table, noting every tagged type reachable from them under sink `sink`. */
  final def uniquesEscapeWalk(sink: String, trees: Iterator[Tree]): Unit =
    if (uniquesEscapeCheckOn) escapeCheck.walk(sink, trees)

  final def uniquesEscapeReport(): Unit = if (uniquesEscapeCheckOn) escapeCheck.report()

  private[this] val escapeStacks: Boolean = java.lang.Boolean.getBoolean("scala.uniques.escapeCheck.stacks")
  /** `-Dscala.uniques.escapeCheck.stacks=true`: remember where each closed-scope type was returned uninterned. */
  final def uniquesEscapeCreated(tp: Type): Unit =
    if (escapeStacks && isClosedWeakLabel(tp.uniquesEscapeTag)) tp.uniquesEscapeStack = new Throwable(s"created in phase $phase")
  final def uniquesEscapeCreationStack(tp: Type): Throwable = tp.uniquesEscapeStack

  private[this] var reuseStacks = 0
  /** DO NOT MERGE: outside any closed scope, `unique` returned `r`: report (a few times) if it, or a direct part of it,
   *  was returned uninterned by a closed scope, with where that instance was made. */
  final def uniquesEscapeOnReuse(r: Type): Unit = if (reuseStacks < 4) {
    var found: Type = null
    if (isClosedWeakLabel(r.uniquesEscapeTag)) found = r
    else foreachPart(r, p => if ((found eq null) && isClosedWeakLabel(p.uniquesEscapeTag)) found = p)
    if (found ne null) {
      reuseStacks += 1
      val e = new Throwable(s"[uniques-escape] closed-scope type $found (made in ${WeakNames(found.uniquesEscapeTag)}) ${if (found eq r) "returned" else "a part of " + r + " returned"} by unique outside closed scopes (label ${WeakNames(scratch.frame.weakLabel)}, phase $phase)")
      e.setStackTrace(e.getStackTrace.take(50))
      e.printStackTrace(System.out)
      val made = found.uniquesEscapeStack
      if (made ne null) {
        System.out.println(s"  ...that instance was ${made.getMessage}:")
        made.getStackTrace.iterator.map(_.toString).filterNot(s => s.contains("$anonfun") || s.contains("Function") || s.startsWith("scala.collection.") || s.contains("UniquesCensus")).take(30).foreach(s => System.out.println("    " + s))
      }
    }
  }

  private[this] var storeStacks = 0
  /** `info` is being stored as `sym`'s info: if a part of it was returned uninterned by a closed scope, print where
   *  (the first few times), to find the path by which it escapes. */
  final def uniquesEscapeOnStore(sym: Symbol, info: Type): Unit = if (storeStacks < 8 && !(sym.isParameter || sym.isExistential || sym.isRefinementClass || sym.isSkolem || sym.owner.isRefinementClass)) {
    var found: Type = null
    def look(tp: Type, depth: Int): Unit =
      if ((found eq null) && (tp ne null) && depth < 12) {
        if (isClosedWeakLabel(tp.uniquesEscapeTag)) found = tp
        else foreachPart(tp, look(_, depth + 1))
      }
    look(info, 0)
    if (found ne null) {
      storeStacks += 1
      val e = new Throwable(s"[uniques-escape] closed-scope type $found (made in ${WeakNames(found.uniquesEscapeTag)}) stored in ${if (sym eq NoSymbol) "a tree's type" else "the info of " + sym.fullNameString} (weak label now ${WeakNames(scratch.frame.weakLabel)}, phase $phase)")
      e.setStackTrace(e.getStackTrace.take(60))
      e.printStackTrace(System.out)
      val made = found.uniquesEscapeStack
      if (made ne null) {
        System.out.println(s"  ...the stored instance was ${made.getMessage}:")
        made.getStackTrace.iterator.map(_.toString).filterNot(s => s.contains("$anonfun") || s.contains("Function") || s.startsWith("scala.collection.") || s.contains("UniquesCensus")).take(45).foreach(s => System.out.println("    " + s))
      }
    }
  }

  private final class EscapeCheck {
    // (sink, label) -> count; sink -> examples of closed-scope escapes
    private[this] val counts = new mutable.HashMap[(String, Int), Long]
    private[this] val examples = new mutable.HashMap[String, mutable.ArrayBuffer[String]]

    def note(sink: String, tp: Type, via: String): Unit = {
      val label = tp.uniquesEscapeTag.toInt
      counts((sink, label)) = counts.getOrElse((sink, label), 0L) + 1
      if (isClosedWeakLabel(label)) {
        val ex = examples.getOrElseUpdate(sink, mutable.ArrayBuffer.empty)
        if (ex.size < 12) {
          ex += s"${tp.getClass.getSimpleName} ${tp}${if (via.isEmpty) "" else s"   <- $via"}"
          val st = uniquesEscapeCreationStack(tp)
          if (st ne null) ex ++= (st.getMessage +: st.getStackTrace.iterator.map(_.toString).filterNot(s => s.contains("$anonfun") || s.contains("Function") || s.contains("withPhase") || s.startsWith("scala.collection.") || s.contains("TypeMaps$") || s.contains("Types$TypeRef") || s.contains("copyTypeRef") || s.contains(".typeRef")).take(40).toList).map("        " + _)
        }
      }
    }

    def walk(sink: String, trees: Iterator[Tree]): Unit = {
      val seenTypes = new java.util.IdentityHashMap[Type, AnyRef]
      val seenSyms = new java.util.IdentityHashMap[Symbol, AnyRef]
      val todo = new java.util.ArrayDeque[Symbol]
      var via = ""
      def sym(s: Symbol): Unit =
        if ((s ne null) && (s ne NoSymbol) && seenSyms.put(s, s) == null) todo.add(s)
      def tpe(tp: Type): Unit =
        if ((tp ne null) && seenTypes.put(tp, tp) == null) {
          if (tp.uniquesEscapeTag != 0) note(sink, tp, via)
          tp match {
            case TypeRef(pre, s, args)          => tpe(pre); args.foreach(tpe); sym(s)
            case SingleType(pre, s)             => tpe(pre); sym(s)
            case ThisType(s)                    => sym(s)
            case SuperType(a, b)                => tpe(a); tpe(b)
            case TypeBounds(lo, hi)             => tpe(lo); tpe(hi)
            case ci: ClassInfoType              => ci.parents.foreach(tpe); ci.decls.foreach(sym); sym(ci.typeSymbol)
            case rt: RefinedType                => rt.parents.foreach(tpe); rt.decls.foreach(sym); sym(rt.typeSymbol)
            case MethodType(params, res)        => params.foreach(sym); tpe(res)
            case PolyType(tparams, res)         => tparams.foreach(sym); tpe(res)
            case NullaryMethodType(res)         => tpe(res)
            case ExistentialType(q, u)          => q.foreach(sym); tpe(u)
            case AnnotatedType(annots, u)       => annots.foreach(a => tpe(a.atp)); tpe(u)
            case ErasedValueType(c, u)          => sym(c); tpe(u)
            case ConstantType(c)                => if (c.tag == ClazzTag) tpe(c.typeValue)
            case tv: TypeVar                    => tpe(tv.inst); tv.constr.loBounds.foreach(tpe); tv.constr.hiBounds.foreach(tpe)
            case OverloadedType(pre, alts)      => tpe(pre); alts.foreach(sym)
            case _                              =>
          }
        }
      def drain(): Unit =
        while (!todo.isEmpty) {
          val s = todo.poll()
          via = s.fullNameString
          s.foreachInfoInHistory(tpe)
          s.annotations.foreach(a => tpe(a.atp))
          sym(s.owner)
        }
      trees.foreach { t =>
        t.foreach { x =>
          via = s"tree ${x.getClass.getSimpleName} at ${x.pos}"
          tpe(x.tpe)
          if (x.hasSymbolField) sym(x.symbol)
          drain()
        }
      }
      sym(rootMirror.RootClass)
      drain()
      walked += s"$sink: ${seenTypes.size} types, ${seenSyms.size} symbols"
    }
    private[this] val walked = mutable.ArrayBuffer.empty[String]

    def report(): Unit = {
      val closed = counts.iterator.filter { case ((_, l), _) => isClosedWeakLabel(l) }.map(_._2).sum
      System.out.println(s"[uniques-escape] walked ${walked.mkString("; ")}")
      System.out.println(s"[uniques-escape] uninterned types found, by sink and scope (closed: ${WeakNames.indices.filter(isClosedWeakLabel).map(WeakNames(_)).mkString(",")}); closed-scope escapes: $closed")
      counts.toList.sortBy { case ((s, l), _) => (s, l) }.foreach { case ((s, l), n) =>
        System.out.println(f"[uniques-escape]   $s%-22s ${WeakNames(l)}%-12s ${if (isClosedWeakLabel(l)) "CLOSED" else "open"}%-6s $n%,d")
      }
      examples.foreach { case (s, ex) => ex.foreach(e => System.out.println(s"[uniques-escape]   e.g. $s: $e")) }
      counts.clear(); examples.clear(); walked.clear()
    }
  }
}

object UniquesCensus {
  // The totals over every Global in this JVM
  private[this] val jvmCounters = new mutable.TreeMap[String, Long]
  private[this] val jvmProbes = new mutable.HashMap[(String, String), Long]

  private def merge(counters: collection.Map[String, Long], last: collection.Map[String, Long],
                    probes: collection.Map[(String, String), Long], lastProbes: collection.Map[(String, String), Long]): (collection.Map[String, Long], collection.Map[(String, String), Long]) =
    synchronized {
      for ((k, n) <- counters) { val d = n - last.getOrElse(k, 0L); if (d != 0) jvmCounters(k) = jvmCounters.getOrElse(k, 0L) + d }
      for ((k, n) <- probes) { val d = n - lastProbes.getOrElse(k, 0L); if (d != 0) jvmProbes(k) = jvmProbes.getOrElse(k, 0L) + d }
      (jvmCounters.clone(), jvmProbes.clone())
    }

  /** Write the JVM's totals to `audit-<pid>.json`, perflab-agent's report format, keeping 200 sites per probe. */
  private def write(totals: (collection.Map[String, Long], collection.Map[(String, String), Long])): File = {
    val (counters, probes) = totals
    val pid = java.lang.management.ManagementFactory.getRuntimeMXBean.getName.takeWhile(_ != '@')
    val dir = Option(System.getProperty("scala.uniques.census.out")).orElse(Option(System.getProperty("perflab.cell"))).getOrElse(".")
    val out = new File(dir, s"audit-$pid.json")
    def esc(s: String) = s.replace("\\", "\\\\").replace("\"", "\\\"")
    val w = new PrintWriter(out, "UTF-8")
    try {
      w.print("{\"counters\": {")
      w.print(counters.toList.sortBy(_._1).map { case (k, n) => "\"" + k + "\": " + n }.mkString(", "))
      w.print("}, \"probes\": {")
      val byProbe = probes.toList.groupBy(_._1._1).toList.sortBy(_._1)
      w.print(byProbe.map { case (name, entries) =>
        val sites = entries.map { case ((_, site), n) => (site, n) }.sortBy(-_._2)
        "\"" + name + "\": {\"kind\": \"sites\", \"hits\": " + sites.map(_._2).sum + ", \"sample\": 1, \"sites\": {" +
          sites.take(200).map { case (k, n) => "\"" + esc(k) + "\": " + n }.mkString(", ") + "}}"
      }.mkString(", "))
      w.println("}}")
    } finally w.close()
    out
  }

  final val MaxClasses = 64
  final val MaxPhases = 64
  final val ClassMask = MaxClasses - 1
  final val PhaseShift = 6
  final val PhaseMask = MaxPhases - 1
  final val CtxShift = 12
  final val ShapeShift = 13
  final val SrcShift = 15
  final val SrcMask = 7
  final val Anchored = 1 << 18
  final val SameFrame = 1 << 19
  final val Counted = 1 << 20
  final val WeakShift = 21
  final val WeakMask = 15
  final val Promoted = 1 << 25
  final val AnchorSiteShift = 26
  final val AnchorSiteMask = (1 << 6) - 1

  final val UncacheTableHit = 0
  final val UncacheCacheHit = 1
  final val UncacheMiss = 2

  final val WeakQuery = 1
  final val WeakMapParams = 2
  final val WeakAdaptPoly = 3
  final val WeakNarrow = 4
  final val WeakExistential = 5
  final val WeakMixin = 6
  final val WeakInstantiate = 7
  final val WeakStableType = 8
  final val WeakOverrideCheck = 9
  final val WeakMemberLookup = 10
  // strong-scope candidates: computations whose types may live for the run (census labels only, never uncached)
  final val StrongLoad = 11
  final val StrongMemberCompletion = 12
  final val StrongInfoTransform = 13
  final val WeakLabels = 14
  val WeakNames = Array("none", "query", "mapParams", "adaptPoly", "narrow", "existential", "mixin", "instantiate", "stableType", "overrideCheck", "memberLookup", "load", "memberCompletion", "infoTransform")
  final val StrongCandidateLabels: Int = (1 << StrongLoad) | (1 << StrongMemberCompletion) | (1 << StrongInfoTransform)
  /** DO NOT MERGE: the closed weak scopes, whose new types should be garbage when they return: the boolean queries
   *  (`isSubType`, `isSameType`, `matchesType`, `specializesSym`, existential subtyping among them), the override checks
   *  (`RefChecks.checkOverride`, erasure's `checkNoDoubleDefs`, `OverridingPairs`' match of a pair) and member lookup's
   *  test for a new member (`FindMembers.isNewMember`). The rest are open:
   *  they return the types they make. */
  final val ClosedWeakLabels: Int = (1 << WeakQuery) | (1 << WeakOverrideCheck) | (1 << WeakMemberLookup)
  def isClosedWeakLabel(label: Int): Boolean = label > 0 && ((ClosedWeakLabels >>> label) & 1) != 0
  /** Frames that only plumb an intern or an anchor, left out of a site. */
  val CreateSkip = java.util.regex.Pattern.compile("^(java\\.|scala\\.reflect\\.internal\\.(tpe\\.UniquesCensus|util\\.ChunkedInterner|Types\\$PerRunUniques|Types\\.unique|Types\\$[A-Za-z]*(TypeRef|Type|Bounds)\\$?\\.(apply|<init>)|Types\\.(typeRef|singleType|newTypeRef|copyTypeRef|TypeBounds|ThisType|ConstantType)))")
  val AnchorSkip = java.util.regex.Pattern.compile("^(java\\.|scala\\.reflect\\.internal\\.(tpe\\.UniquesCensus|Symbols\\$[A-Za-z]*Symbol\\.(info_\\$eq|setInfo|updateInfo|setTyconCache|transformInfos|setInfoOwnerAdjusted|setInfoAndEnter|modifyInfo)))")

  final val ClearedFree = 0
  final val LiveFree = 1
  final val AnchoredSame = 2
  final val AnchoredLater = 3
  final val AnchoredCleared = 4
  final val Fates = 5
  val FateNames = Array("clearedFree", "liveFree", "anchoredSame", "anchoredLater", "anchoredCleared")

  final val SrcNone = 0
  final val SrcInfo = 1 // resolved by the symbol into one of the next four
  final val SrcMember = 1
  final val SrcParam = 2
  final val SrcLocal = 3
  final val SrcOther = 4
  final val SrcUpdate = 5
  final val SrcTransform = 6
  final val SrcCache = 7
  final val Srcs = 8
  val SrcNames = Array("none", "member", "param", "local", "other", "update", "transform", "cache")

  val CtxNames = Array("outside", "completion")
  val ShapeNames = Array("plain", "methodParam", "skolemOrExistential", "both")
}
