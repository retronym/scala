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

  private[this] lazy val census = new Census

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
      ref.census = cls | (ph << PhaseShift) | (ctx << CtxShift) | (sh << ShapeShift)
      ref.serial = frame.serial
      addedCls((cls * 2 + ctx) * 4 + sh) += 1
      addedPhase((ph * 2 + ctx) * 4 + sh) += 1
    }

    def cleared(ref: ChunkedInterner.Ref): Unit = {
      val c = ref.census
      if ((c & Counted) == 0) {
        ref.census = c | Counted
        val fate = if ((c & Anchored) != 0) AnchoredCleared else ClearedFree
        count(c, fate)
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
      def visit(t: Type): Unit = {
        anchorVisits += 1
        t match {
          case u: UniqueType =>
            val r = table.refOf(u, u.hashCode)
            if (r ne null) {
              val c = r.census
              if ((c & Anchored) == 0) {
                r.census = c | Anchored | (if (r.serial == serial) SameFrame else 0) | (src << SrcShift)
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
        }
      }
      table.listener = null
      table = null
      runs += 1
    }

    def report(): Unit = {
      finish()
      val counters = new mutable.TreeMap[String, Long]
      def add(k: String, n: Long): Unit = if (n != 0) counters(k) = counters.getOrElse(k, 0L) + n
      add("runs", runs)
      add("anchor.walks", anchorWalks)
      add("anchor.visits", anchorVisits)
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
      val pid = java.lang.management.ManagementFactory.getRuntimeMXBean.getName.takeWhile(_ != '@')
      val dir = Option(System.getProperty("scala.uniques.census.out")).orElse(Option(System.getProperty("perflab.cell"))).getOrElse(".")
      val out = new File(dir, s"audit-$pid.json")
      val w = new PrintWriter(out, "UTF-8")
      try {
        w.print("{\"counters\": {")
        w.print(counters.iterator.map { case (k, n) => "\"" + k + "\": " + n }.mkString(", "))
        w.println("}, \"probes\": {}}")
      } finally w.close()
      def c(k: String) = counters.getOrElse(k, 0L)
      System.out.println(s"[uniques-census] runs=$runs added=${c("added")} anchoredSame=${c("fate.anchoredSame")} anchoredLater=${c("fate.anchoredLater")} anchoredCleared=${c("fate.anchoredCleared")} clearedFree=${c("fate.clearedFree")} liveFree=${c("fate.liveFree")} -> $out")
    }
  }
}

object UniquesCensus {
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
