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

// todo implement in terms of BitSet
import scala.collection.mutable
import util.Statistics

/** A base type sequence (BaseTypeSeq) is an ordered sequence spanning all the base types
 *  of a type. It characterized by the following two laws:
 *
 *  (1) Each element of `tp.baseTypeSeq`  is a basetype of `tp`
 *  (2) For each basetype `bt1` of `tp` there is an element `bt` in `tp.baseTypeSeq` such that
 *
 *      bt.typeSymbol = bt1.typeSymbol
 *      bt <: bt1
 *
 *  (3) The type symbols of different elements are different.
 *
 *  Elements in the sequence are ordered by Symbol.isLess.
 *  @note base type sequences were called closures up to 2.7.1. The name has been changed
 *  to avoid confusion with function closures.
 */
object BaseTypeIndexStats {
  final val enabled = true
  private final val Max = 256
  val lenHit, lenMiss, idxHit, fromEndHit, lenMissMaskReject = new Array[Long](Max + 1)
  var scanIters, maskedIters, hitLenSum, missLenSum = 0L
  var calls, hits, misses, maskFalsePos, nonClassTarget, repeatSame, sameBts = 0L
  private var inOp = false
  private def reset(): Unit = {
    Seq(lenHit, lenMiss, idxHit, fromEndHit, lenMissMaskReject).foreach(java.util.Arrays.fill(_, 0L))
    lenKnown = 0; lenMissReject = 0; lenMissRejectAfterMask = 0; maskFPKnown = 0; violations = 0; hitFwdIters = 0; hitBackIters = 0; hitsKnown = 0; missBackIters = 0; missFwdIters = 0; missesKnownNotRejected = 0
    scanIters = 0; maskedIters = 0; hitLenSum = 0; missLenSum = 0; calls = 0; hits = 0; misses = 0; maskFalsePos = 0; nonClassTarget = 0; repeatSame = 0; sameBts = 0; callers.clear()
  }
  private var lastBts: AnyRef = null
  private var lastId = -1
  private val callers = new java.util.HashMap[String, Array[Long]]()
  private def b(n: Int) = if (n > Max) Max else n
  def record(bts: AnyRef, id: Int, len: Int, res: Int, maskHit: Boolean, isClass: Boolean): Unit = {
    calls += 1
    if (inOp && calls == 50000000L) { dump(); }
    if (!inOp && (calls & ((1 << 23) - 1)) == 0) { System.err.println(s"[bti] cumulative at $calls"); dump() }
    if (!inOp && (calls & 4095) == 0 && new Throwable().getStackTrace.exists(f => f.getClassName == "scala.tools.nsc.compilerlab.SymbolOps" || f.getClassName.startsWith("scala.tools.nsc.compilerlab.SymbolOps$"))) {
      dump(); reset(); inOp = true
    }
    if (!isClass) nonClassTarget += 1
    if (bts eq lastBts) { sameBts += 1; if (id == lastId) repeatSame += 1 }
    lastBts = bts; lastId = id
    val iters = if (res >= 0) res + 1 else len
    scanIters += iters; if (res >= 0 || maskHit) maskedIters += iters
    if (res >= 0) hitLenSum += len else missLenSum += len
    if (res >= 0) { hits += 1; lenHit(b(len)) += 1; idxHit(b(res)) += 1; fromEndHit(b(len - 1 - res)) += 1 }
    else { misses += 1; lenMiss(b(len)) += 1; if (maskHit) maskFalsePos += 1 else lenMissMaskReject(b(len)) += 1 }
    if ((calls & 1023) == 0) {
      val st = new Throwable().getStackTrace.iterator.drop(1).filterNot(f => f.getMethodName.startsWith("baseTypeIndex") || f.getMethodName == "record").take(3).map(f => f.getClassName.replaceAll(".*\\.", "") + "." + f.getMethodName + ":" + f.getLineNumber)
      val site = st.mkString(" <- ")
      val c = callers.computeIfAbsent(site, _ => new Array[Long](2)); c(if (res >= 0) 0 else 1) += 1
    }
  }
  var lenKnown, lenMissReject, lenMissRejectAfterMask, maskFPKnown, violations, hitFwdIters, hitBackIters, hitsKnown, missBackIters, missFwdIters, missesKnownNotRejected = 0L
  def recordLen(len: Int, res: Int, lc: Int, maskHit: Boolean): Unit = if (lc >= 0) {
    lenKnown += 1
    val bound = len - lc // the last index the target may sit at
    if (res >= 0) {
      hitsKnown += 1
      if (res > bound) violations += 1
      hitFwdIters += res + 1
      hitBackIters += (bound - res + 1).max(1)
    } else {
      if (maskHit) maskFPKnown += 1
      if (bound < 0 || (bound == 0 && len > 0)) { lenMissReject += 1; if (maskHit) lenMissRejectAfterMask += 1 }
      else { missesKnownNotRejected += 1; missFwdIters += len; missBackIters += bound + 1 }
    }
  }
  private def hist(name: String, a: Array[Long]): String = {
    val tot = a.sum.max(1)
    a.indices.filter(a(_) > 0).map(i => f"$i%s:${a(i)}%d(${100.0 * a(i) / tot}%.1f%%)").mkString(s"$name [", " ", "]")
  }
  def dump(): Unit = {
    val sw = new java.io.StringWriter; val o = new java.io.PrintWriter(sw)
    o.println(if (inOp) "[bti] ---- op ----" else "[bti] ---- setup (compile) ----")
    o.println(s"[bti] calls=$calls hits=$hits misses=$misses maskFalsePos=$maskFalsePos nonClassTarget=$nonClassTarget sameBtsAsPrev=$sameBts sameBtsAndSym=$repeatSame")
    o.println(f"[bti] meanLenHit=${hitLenSum.toDouble / hits.max(1)}%.1f meanLenMiss=${missLenSum.toDouble / misses.max(1)}%.1f scanItersPerCall=${scanIters.toDouble / calls.max(1)}%.2f maskedScanItersPerCall=${maskedIters.toDouble / calls.max(1)}%.2f")
    o.println(f"[bti] classLen: known=$lenKnown hitsKnown=$hitsKnown violations=$violations hitIters fwd=${hitFwdIters.toDouble / hitsKnown.max(1)}%.2f backFromBound=${hitBackIters.toDouble / hitsKnown.max(1)}%.2f | missesKnown=${lenKnown - hitsKnown} lenRejects=$lenMissReject maskFP=$maskFPKnown maskFPAlsoLenRejected=$lenMissRejectAfterMask notRejected=$missesKnownNotRejected missIters fwd=${missFwdIters.toDouble / missesKnownNotRejected.max(1)}%.2f back=${missBackIters.toDouble / missesKnownNotRejected.max(1)}%.2f")
    o.println("[bti] " + hist("lenHit", lenHit)); o.println("[bti] " + hist("lenMiss", lenMiss))
    o.println("[bti] " + hist("idxHit", idxHit)); o.println("[bti] " + hist("fromEndHit", fromEndHit))
    o.println("[bti] " + hist("lenMissMaskReject", lenMissMaskReject))
    import scala.jdk.CollectionConverters._
    callers.asScala.toList.sortBy(-_._2.sum).take(25).foreach { case (k, v) => o.println(s"[bti] site(1/1024) hit=${v(0)} miss=${v(1)} $k") }
    o.flush(); System.err.print(sw.toString); System.err.flush()
  }
}

trait BaseTypeSeqs {
  this: SymbolTable =>
  import definitions._
  import statistics._

  protected def newBaseTypeSeq(parents: List[Type], elems: Array[Type]) =
    new BaseTypeSeq(parents, elems)

  protected def newMappedBaseTypeSeq(orig: BaseTypeSeq, f: Type => Type) =
    new MappedBaseTypeSeq(orig, f)

  /** Note: constructor is protected to force everyone to use the factory method newBaseTypeSeq instead.
   *  This is necessary because when run from reflection every base type sequence needs to have a
   *  SynchronizedBaseTypeSeq as mixin.
   */
  class BaseTypeSeq protected[reflect] (private[BaseTypeSeqs] val parents: List[Type], private[BaseTypeSeqs] val elems: Array[Type]) {
  self =>
    if (settings.areStatisticsEnabled) statistics.incCounter(baseTypeSeqCount)
    if (settings.areStatisticsEnabled) statistics.incCounter(baseTypeSeqLenTotal, elems.length)
    private[this] val typeSymbols = {
      val tmp = new Array[Int](elems.length)
      var i = 0
      while (i < elems.length) {
        tmp(i) = elems(i).typeSymbol.id
        i += 1
      }
      tmp
    }

    /** The number of types in the sequence */
    def length: Int = elems.length

    // #3676 shows why we can't store NoType in elems to mark cycles
    // (while NoType is in there to indicate a cycle in this BTS, during the execution of
    //  the mergePrefixAndArgs below, the elems get copied without the pending map,
    //  so that NoType's are seen instead of the original type --> spurious compile error)
    private[this] val pending = new mutable.BitSet(length)

    /** The type at i'th position in this sequence; lazy types are returned evaluated. */
    def apply(i: Int): Type =
      if(pending contains i) {
        pending.clear()
        throw CyclicInheritance
      } else {
        def computeLazyType(rtp: RefinedType): Type = {
          devWarningIf(!isIntersectionTypeForLazyBaseType(rtp)) {
            "unexpected RefinedType in base type seq, lazy BTS elements should be created via intersectionTypeForLazyBaseType: " + rtp
          }
          val variants = rtp.parents
          // can't assert decls.isEmpty; see t0764
          //if (!decls.isEmpty) abort("computing closure of "+this+":"+this.isInstanceOf[RefinedType]+"/"+closureCache(j))
          //Console.println("compute closure of "+this+" => glb("+variants+")")
          pending += i
          try {
            mergePrefixAndArgs(variants, Variance.Contravariant, lubDepth(variants)) match {
              case NoType => typeError("no common type instance of base types " + (variants mkString ", and ") + " exists.")
              case tp0 =>
                pending(i) = false
                elems(i) = tp0
                tp0
            }
          }
          catch {
            case CyclicInheritance =>
              typeError(
                "computing the common type instance of base types " + (variants mkString ", and ") + " leads to a cycle.")
          }
        }
        elems(i) match {
          case rtp@RefinedType(variants, decls) =>
            computeLazyType(rtp)
          case et @ ExistentialType(quantified, rtp: RefinedType) =>
            existentialAbstraction(quantified, computeLazyType(rtp))
          case tp =>
            tp
        }
      }

    def rawElem(i: Int) = elems(i)

    /** The type symbol of the type at i'th position in this sequence */
    def typeSymbol(i: Int): Symbol = elems(i).typeSymbol

    final def baseTypeIndex(sym: Symbol): Int = {
      val symId = sym.id
      var i = 0
      val len = length
      var res = -1
      while (i < len && res < 0) {
        if (typeSymbols(i) == symId) res = i
        i += 1
      }
      if (BaseTypeIndexStats.enabled) {
        var mask = 0L
        var j = 0
        while (j < len) { mask |= 1L << typeSymbols(j); j += 1 }
        val lc =
          if (sym.isClass && !sym.isPackageClass && len > 0 && elems(0).typeSymbol.isClass && sym.rawInfo.isComplete) sym.info.baseTypeSeq.length
          else -1
        BaseTypeIndexStats.record(this, symId, len, res, (mask & (1L << symId)) != 0L, sym.isClass)
        BaseTypeIndexStats.recordLen(len, res, lc, (mask & (1L << symId)) != 0L)
      }
      res
    }

    /** Return all evaluated types in this sequence as a list */
    def toList: List[Type] = elems.toList

    /** Return an iterator over all evaluated types in this sequence */
    def toIterator: Iterator[Type] = elems.iterator

    def copy(head: Type, offset: Int): BaseTypeSeq = {
      val arr = new Array[Type](elems.length + offset)
      java.lang.System.arraycopy(elems, 0, arr, offset, elems.length)
      arr(0) = head
      newBaseTypeSeq(parents, arr)
    }

    /** Compute new base type sequence with `tp` prepended to this sequence */
    def prepend(tp: Type): BaseTypeSeq = copy(tp, 1)

    /** Compute new base type sequence with `tp` replacing the head of this sequence */
    def updateHead(tp: Type): BaseTypeSeq = copy(tp, 0)

    /** Compute new base type sequence where every element is mapped
     *  with function `f`. Lazy types are mapped but not evaluated */
    def map(f: Type => Type): BaseTypeSeq = {
	  // inlined `elems map f` for performance
      val len = length
      val arr = new Array[Type](len)
      var i = 0
      while (i < len) {
        arr(i) = f(elems(i))
        i += 1
      }
      newBaseTypeSeq(parents, arr)
    }

    def lateMap(f: Type => Type): BaseTypeSeq = newMappedBaseTypeSeq(this, f)

    def exists(p: Type => Boolean): Boolean = elems exists p

    lazy val maxDepth = maxDepthOfElems

    protected def maxDepthOfElems: Depth = {
      var d = Depth.Zero
      1 until length foreach (i => d = d max typeDepth(elems(i)))
      d
    }

    override def toString = elems.mkString("BTS(", ",", ")")

    private def typeError(msg: String): Nothing =
      throw new TypeError(
        "the type intersection "+(parents mkString " with ")+" is malformed"+
        "\n --- because ---\n"+msg)
  }

  /** A marker object for a base type sequence that's no yet computed.
   *  used to catch inheritance cycles
   */
  lazy val undetBaseTypeSeq: BaseTypeSeq = newBaseTypeSeq(List(), Array())

  /** Create a base type sequence consisting of a single type */
  def baseTypeSingletonSeq(tp: Type): BaseTypeSeq = newBaseTypeSeq(List(), Array(tp))

  /** Create the base type sequence of a compound type with given tp.parents */
  def compoundBaseTypeSeq(tp: Type): BaseTypeSeq = {
    val tsym = tp.typeSymbol
    val parents = tp.parents
//    Console.println("computing baseTypeSeq of " + tsym.tpe + " " + parents)//DEBUG
    val buf = new mutable.ListBuffer[Type]
    buf += tsym.tpe_*
    var btsSize = 1
    if (parents.nonEmpty) {
      val nparents = parents.length
      val pbtss = new Array[BaseTypeSeq](nparents)
      val index = new Array[Int](nparents)
      var i = 0
      for (p <- parents) {
        val parentBts = p.dealias.baseTypeSeq // dealias need for scala/bug#8046.
        pbtss(i) =
          if (parentBts eq undetBaseTypeSeq) AnyClass.info.baseTypeSeq
          else parentBts
        index(i) = 0
        i += 1
      }
      def nextTypeSymbol(i: Int): Symbol = {
        val j = index(i)
        val pbts = pbtss(i)
        if (j < pbts.length) pbts.typeSymbol(j) else AnyClass
      }
      def nextRawElem(i: Int): Type = {
        val j = index(i)
        val pbts = pbtss(i)
        if (j < pbts.length) pbts.rawElem(j) else AnyTpe
      }
      var minSym: Symbol = NoSymbol
      while (minSym != AnyClass) {
        minSym = nextTypeSymbol(0)
        i = 1
        while (i < nparents) {
          val nextSym = nextTypeSymbol(i)
          if (nextSym isLess minSym)
            minSym = nextSym
          i += 1
        }
        var minTypes: List[Type] = List()
        def updateInMinTypes(tp: Type): Unit = {
          def loop(tps: List[Type]): Boolean = tps match {
            case x :: tps => (tp =:= x) || loop(tps)
            case _        => false
          }
          if (!loop(minTypes))
            minTypes ::= tp
        }

        i = 0
        while (i < nparents) {
          if (nextTypeSymbol(i) == minSym) {
            nextRawElem(i) match {
              case RefinedType(variants, _) => variants.foreach(updateInMinTypes)
              case ntp => updateInMinTypes(ntp)
            }
            index(i) = index(i) + 1
          }
          i += 1
        }
        buf += intersectionTypeForLazyBaseType(minTypes) // TODO this reverses the order. Does this matter? Or should this be minTypes.reverse?
        btsSize += 1
      }
    }
    val elems = new Array[Type](btsSize)
    @annotation.unused val copied = buf.copyToArray(elems, 0)
    //assert(copied == btsSize, "array copied")
//    Console.println("computed baseTypeSeq of " + tsym.tpe + " " + parents + ": "+elems.toString)//DEBUG
    newBaseTypeSeq(parents, elems)
  }

  class MappedBaseTypeSeq(orig: BaseTypeSeq, f: Type => Type) extends BaseTypeSeq(orig.parents map f, orig.elems) {
    override def apply(i: Int) = f(orig.apply(i))
    override def rawElem(i: Int) = f(orig.rawElem(i))
    override def toList = orig.toList map f
    override def copy(head: Type, offset: Int) = (orig map f).copy(head, offset)
    override def map(g: Type => Type) = lateMap(g)
    override def lateMap(g: Type => Type) = orig.lateMap(x => g(f(x)))
    override def exists(p: Type => Boolean) = elems exists (x => p(f(x)))
    override protected def maxDepthOfElems: Depth = elems.map(x => typeDepth(f(x))).max
    override def toString = elems.mkString("MBTS(", ",", ")")
  }

  val CyclicInheritance = new Throwable
}

trait BaseTypeSeqsStats {
  self: Statistics =>
  val baseTypeSeqCount = newCounter("#base type seqs")
  val baseTypeSeqLenTotal = newRelCounter("avg base type seq length", baseTypeSeqCount)
}
