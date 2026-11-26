// Range and NumericRange against a model in BigInt arithmetic that never materialises a long range.
// usage: RangeDiff <seed> <seconds> [skip=dom:label,...]
import scala.collection.immutable.NumericRange
import scala.util.Random

final case class V(b: BigInt) // a model element (scaled integer for BigDecimal domains)
final class ME(val n: String) extends RuntimeException(n) // exception the model expects

abstract class Dom(val name: String, val lo: Option[BigInt], val hi: Option[BigInt], val bits: Int) {
  type T
  def from(b: BigInt): T
  def big(t: T): BigInt
  def mk(s: T, e: T, st: T, incl: Boolean): IndexedSeq[T]
  def num: Numeric[T]
  def show(b: BigInt): String = b.toString
  def clamp(b: BigInt): BigInt = { val a = lo.fold(b)(_ max b); hi.fold(a)(_ min a) }
  def wrap(b: BigInt): BigInt = (lo, hi) match { case (Some(l), Some(h)) => ((b - l) mod (h - l + 1)) + l; case _ => b }
  def isRange = false
  def anchors: Vector[BigInt] = Vector(lo.get, hi.get, BigInt(0), lo.get / 2, hi.get / 2)
  def slow = false // sum / contains may be linear
  def ord: Ordering[T] = num // the ordering for which min and max are constant time
}
object Dom {
  private def b(i: Long) = Some(BigInt(i))
  val IntRange = new Dom("Range", b(Int.MinValue), b(Int.MaxValue), 32) {
    type T = Int; def from(x: BigInt) = x.toInt; def big(t: Int) = BigInt(t); def num = implicitly
    def mk(s: Int, e: Int, st: Int, incl: Boolean) = if (incl) Range.inclusive(s, e, st) else Range(s, e, st)
    override def isRange = true
    override def ord = Ordering.Int
  }
  class NR[A](n: String, l: Option[BigInt], h: Option[BigInt], bts: Int, f: BigInt => A, g: A => BigInt)(implicit val int: Integral[A]) extends Dom(n, l, h, bts) {
    type T = A; def from(x: BigInt) = f(x); def big(t: A) = g(t); def num = int
    def mk(s: A, e: A, st: A, incl: Boolean) = if (incl) NumericRange.inclusive(s, e, st) else NumericRange(s, e, st)
  }
  val IntNR = new NR[Int]("NR[Int]", b(Int.MinValue), b(Int.MaxValue), 32, _.toInt, BigInt(_))
  val LongNR = new NR[Long]("NR[Long]", b(Long.MinValue), b(Long.MaxValue), 64, _.toLong, BigInt(_))
  val ShortNR = new NR[Short]("NR[Short]", b(Short.MinValue), b(Short.MaxValue), 16, _.toShort, BigInt(_))
  val ByteNR = new NR[Byte]("NR[Byte]", b(Byte.MinValue), b(Byte.MaxValue), 8, _.toByte, BigInt(_))
  val CharNR = new NR[Char]("NR[Char]", b(0), b(Char.MaxValue), 16, _.toChar, c => BigInt(c.toInt))
  val BigIntNR = new NR[BigInt]("NR[BigInt]", None, None, 70, identity, identity) {
    override def anchors = Vector(BigInt(0), BigInt(Int.MaxValue), BigInt(Int.MinValue), BigInt(Long.MaxValue), BigInt(Long.MinValue), BigInt(1) << 64)
  }
  def bd(scale: Int) = new NR[BigDecimal](s"NR[BigDecimal/$scale]", Some(-BigInt(10).pow(14)), Some(BigInt(10).pow(14)), 46,
    x => BigDecimal(x, scale), t => BigInt(t.bigDecimal.movePointRight(scale).toBigIntegerExact))(Numeric.BigDecimalAsIfIntegral) {
    override def show(x: BigInt) = { val d = new java.math.BigDecimal(x.bigInteger, scale); if (d.signum == 0) "0" else d.stripTrailingZeros.toPlainString }
    override def anchors = Vector(BigInt(0), BigInt(Int.MaxValue), BigInt(Int.MinValue), BigInt(10).pow(scale), -BigInt(10).pow(scale), lo.get, hi.get)
    override def slow = true
    override def wrap(x: BigInt) = x
  }
  val all = Vector[Dom](IntRange, IntRange, IntRange, IntNR, LongNR, LongNR, ShortNR, ByteNR, CharNR, BigIntNR, bd(0), bd(2), bd(5))
}

final case class M(start: BigInt, end: BigInt, step: BigInt, incl: Boolean) {
  val count: BigInt = if (step == 0) 0 else { val d = (end - start) * step.signum; if (d < 0) BigInt(0) else { val a = step.abs; val q = d / a; if (!incl && d % a == 0) q else q + 1 } }
}
// elements i0 until i0 + c of m, possibly read backwards
final case class View(m: M, i0: BigInt, c: BigInt, rev: Boolean) {
  def el(j: BigInt): BigInt = m.start + (if (rev) i0 + c - 1 - j else i0 + j) * m.step
  def sub(a: BigInt, n: BigInt) = if (rev) View(m, i0 + c - a - n, n, true) else View(m, i0 + a, n, false)
  def posOf(x: BigInt): BigInt = { val d = x - m.start; if (c == 0 || d % m.step != 0) -1 else { val k = d / m.step; if (k < i0 || k >= i0 + c) -1 else if (rev) i0 + c - 1 - k else k - i0 } }
  def cl(i: Int): BigInt = BigInt(i) max 0 min c
  def take(i: Int) = sub(0, cl(i)); def drop(i: Int) = sub(cl(i), c - cl(i))
  def slice(i: Int, j: Int) = { val a = cl(i); val b = cl(j) max a; sub(a, b - a) }
  def big = c > Int.MaxValue
  def list(n: Int): List[V] = (0 until n).map(j => V(el(j))).toList
}

object RangeDiff {
  val IAE = "IllegalArgumentException"; val NSE = "NoSuchElementException"; val UOE = "UnsupportedOperationException"; val IOOBE = "IndexOutOfBoundsException"
  var rnd: Random = _
  var d: Dom = _
  @volatile var beat = 0L; @volatile var where = ""
  var cases = 0L; var checks = 0L; var desc = ""; var chain = ""
  val sigs = scala.collection.mutable.LinkedHashMap.empty[String, (Int, List[String])]
  var skip = Set.empty[String]
  var quarantineKnown = false // quarantine=1: leave out inputs that only reproduce findings already recorded

  def render(a: Any): String = a match {
    case V(b) => d.show(b)
    case c: Char => d.show(BigInt(c.toInt))
    case x: BigDecimal => try d.show(d.asInstanceOf[Dom { type T = BigDecimal }].big(x)) catch { case _: Throwable => s"OFFGRID($x)" }
    // drained by hand: mkString and toList consult knownSize, which is one of the things under test
    case it: Iterator[_] => val b = new StringBuilder("["); var n = 0; while (it.hasNext && n < 5000) { if (n > 0) b.append(','); b.append(render(it.next())); n += 1 }; b.append(']').toString
    case it: Iterable[_] => render(it.iterator)
    case Some(x) => "Some(" + render(x) + ")"
    case p: Product if p.productArity > 0 => p.productIterator.map(render).mkString("(", ",", ")")
    case x => String.valueOf(x)
  }
  def ev(f: => Any): String = try render(f) catch { case m: ME => "EX:" + m.n; case _: StackOverflowError => "EX:StackOverflowError"; case e: Exception => "EX:" + e.getClass.getSimpleName }
  def emptyEx(s: String) = if (s == "EX:" + NSE || s == "EX:" + UOE) "EX:empty" else s
  def kind(s: String) = if (s.startsWith("EX:")) s else "val"

  def q(label: String, detail: => String = "", lenient: Boolean = false)(exp: => Any)(act: => Any): Boolean = {
    if (skip(d.name + ":" + label)) return true
    where = s"$desc $chain $label $detail"; beat = System.nanoTime(); checks += 1
    val e = ev(exp); val a = ev(act)
    val ok = e == a || (lenient && a == "EX:" + IAE)
    if (!ok) {
      val sig = s"${d.name} | ${chain.replaceAll("-?\\d+", "#")} | $label | ${kind(e)} -> ${kind(a)}"
      val (n, ex) = sigs.getOrElse(sig, (0, Nil))
      sigs(sig) = (n + 1, if (n < 3) ex :+ s"$desc $chain $label $detail expected ${e.take(200)} got ${a.take(200)}" else ex)
    }
    ok
  }

  def genVal(): BigInt = {
    val small = BigInt(rnd.nextInt(7) - 3)
    val b = rnd.nextInt(10) match {
      case 0 | 1 | 2 => d.anchors(rnd.nextInt(d.anchors.size)) + small
      case 3 | 4 => small * (rnd.nextInt(20) + 1)
      case 5 => (BigInt(1) << rnd.nextInt(d.bits)) * (if (rnd.nextBoolean()) 1 else -1) + small
      case 6 | 7 => BigInt(rnd.nextInt(2001) - 1000)
      case _ => BigInt(d.bits, rnd.self) - (BigInt(1) << (d.bits - 1))
    }
    d.clamp(b)
  }
  def idx(c: BigInt): Int = {
    val n = if (c > Int.MaxValue) Int.MaxValue else c.toInt
    rnd.nextInt(16) match {
      case 0 => -1; case 1 => 0; case 2 => 1; case 3 => 2; case 4 => n - 1; case 5 => n; case 6 => n + 1; case 7 => n / 2
      case 8 => Int.MaxValue; case 9 => Int.MinValue; case 10 => Int.MaxValue - 1; case 11 => n - 2; case 12 => rnd.nextInt(); case _ => rnd.nextInt(n.max(1).min(1000000) + 2)
    }
  }

  def transformers(dd: Dom): Vector[(String, (View, Int, Int) => View, (IndexedSeq[dd.T], Int, Int) => IndexedSeq[dd.T])] = Vector(
    ("take", (v, i, _) => v.take(i), (r, i, _) => r.take(i)),
    ("drop", (v, i, _) => v.drop(i), (r, i, _) => r.drop(i)),
    ("takeRight", (v, i, _) => v.sub(v.c - v.cl(i), v.cl(i)), (r, i, _) => r.takeRight(i)),
    ("dropRight", (v, i, _) => v.sub(0, v.c - v.cl(i)), (r, i, _) => r.dropRight(i)),
    ("slice", (v, i, j) => v.slice(i, j), (r, i, j) => r.slice(i, j)),
    ("splitAt1", (v, i, _) => v.take(i), (r, i, _) => r.splitAt(i)._1),
    ("splitAt2", (v, i, _) => v.drop(i), (r, i, _) => r.splitAt(i)._2),
    ("tail", (v, _, _) => if (v.c == 0) throw new ME(UOE) else v.sub(1, v.c - 1), (r, _, _) => r.tail),
    ("init", (v, _, _) => if (v.c == 0) throw new ME(UOE) else v.sub(0, v.c - 1), (r, _, _) => r.init),
    // NumericRange.reverse throws ArithmeticException by design for Char, and (known finding LN5) for a step of MinValue
    ("reverse", (v, _, _) => if (v.m.step == dd.lo.orNull) throw new ME("skip") else v.copy(rev = !v.rev), (r, _, _) => try r.reverse catch { case _: ArithmeticException if dd.lo.contains(BigInt(0)) => throw new ME("skip") }),
  )

  def queries(dd: Dom)(r: IndexedSeq[dd.T], v: View, top: Boolean): Unit = {
    val c = v.c; val big = v.big; val n = if (big) -1 else c.toInt
    def lenE: Any = if (big) throw new ME(IAE) else n
    def ix(p: BigInt): Any = if (p > Int.MaxValue) -1 else p.toInt // an index beyond Int is reported as absent
    val T = dd.from _
    q("length")(lenE)(r.length); q("size")(lenE)(r.size); q("knownSize")(lenE)(r.knownSize)
    q("isEmpty", lenient = big)(c == 0)(r.isEmpty)
    q("head", lenient = big)(if (c == 0) throw new ME(NSE) else V(v.el(0)))(r.head)
    q("last", lenient = big)(if (c == 0) throw new ME(NSE) else V(v.el(c - 1)))(r.last)
    q("headOption", lenient = big)(if (c == 0) None else Some(V(v.el(0))))(r.headOption)
    q("lastOption", lenient = big)(if (c == 0) None else Some(V(v.el(c - 1))))(r.lastOption)
    val i = idx(c); val j = idx(c)
    q("apply", s"($i)", big)(if (i < 0 || BigInt(i) >= c) throw new ME(IOOBE) else V(v.el(i)))(r(i))
    q("lengthCompare", s"($i)", big)((c compare BigInt(i)).sign)(r.lengthCompare(i).sign)
    val x = { val b = if (c > 0 && rnd.nextInt(4) > 0) v.el(BigInt(idx(c)).max(0).min(c - 1)) + (rnd.nextInt(6) match { case 0 => 1; case 1 => -1; case 2 => v.m.step; case _ => 0 }) else genVal(); if (dd.clamp(b) == b) b else v.m.start }
    val p = v.posOf(x)
    if (!dd.slow || c <= 1000000) {
      q("contains", s"(${dd.show(x)})", big)(p >= 0)(r.contains(T(x)))
      q("indexOf", s"(${dd.show(x)})", big)(ix(p))(r.indexOf(T(x)))
      q("lastIndexOf", s"(${dd.show(x)})", big)(ix(p))(r.lastIndexOf(T(x)))
      q("indexOf.from", s"(${dd.show(x)}, $i)", big)(if (p >= BigInt(i)) ix(p) else -1)(r.indexOf(T(x), i))
      q("lastIndexOf.end", s"(${dd.show(x)}, $i)", big)(if (p <= BigInt(i)) ix(p) else -1)(r.lastIndexOf(T(x), i))
      q("sum", "", big)(V(if (c == 0) 0 else dd.wrap(c * (v.el(0) + v.el(c - 1)) / 2)))(r.sum(dd.num))
    }
    val ord: Ordering[dd.T] = dd.ord
    q("min", "", big)(if (c == 0) "EX:empty" else V(v.el(0) min v.el(c - 1)))(emptyEx(ev(r.min(ord))))
    q("max", "", big)(if (c == 0) "EX:empty" else V(v.el(0) max v.el(c - 1)))(emptyEx(ev(r.max(ord))))
    if (c <= 200000) {
      q("min.rev")(if (c == 0) "EX:empty" else V(v.el(0) max v.el(c - 1)))(emptyEx(ev(r.min(ord.reverse))))
      q("max.rev")(if (c == 0) "EX:empty" else V(v.el(0) min v.el(c - 1)))(emptyEx(ev(r.max(ord.reverse))))
    }
    // iterator with drop/take/slice
    if (if (dd.isRange) c > 0 || !quarantineKnown else c <= 500000) {
      var iv = v; var it: Iterator[dd.T] = null; var ops = "iterator"
      val made = q("iterator", "", big)(())({ it = r.iterator; () }) && it != null
      var k = rnd.nextInt(4); var ok = made
      while (ok && k > 0) {
        // Iterator.slice skips one element at a time, so its lower bound is kept small
        k -= 1; val a0 = idx(iv.c); val b = idx(iv.c); val sl = rnd.nextInt(3) == 2; val a = if ((sl || ops != "iterator") && a0 > 1000000) a0 % 1000 else a0 // likewise drop after take or slice
        (if (sl) 2 else rnd.nextInt(2)) match {
          case 0 => ops += s".drop($a)"; iv = iv.drop(a); it = it.drop(a)
          case 1 => ops += s".take($a)"; iv = iv.take(a); it = it.take(a)
          case _ => ops += s".slice($a,$b)"; iv = iv.slice(a, b); it = it.slice(a, b)
        }
      }
      if (ok) {
        where = s"$desc $chain $ops"
        if (!quarantineKnown) q("it.knownSize", ops)("ok")({ val ks = it.knownSize; if (ks == -1 || BigInt(ks) == iv.c) "ok" else s"knownSize=$ks size=${iv.c}" })
        q("it.hasNext", ops)(iv.c > 0)(it.hasNext)
        if (iv.c <= 300) {
          q("it.drain", ops)(iv.list(iv.c.toInt))(it)
          q("it.exhausted", ops)((false, "EX:" + NSE, 0))((it.hasNext, ev(it.next()), it.knownSize.max(0)))
        } else q("it.take3", ops)(iv.list(3))(it.take(3))
      }
    }
    if (!big && n <= 300) {
      val l: List[dd.T] = (0 until n).map(k => T(v.el(k))).toList
      q("==list")(true)(r == l); q("list==")(true)(l == r); q("hashCode")(l.hashCode)(r.hashCode)
      q("toList")(l)(r.toList); q("reverseIterator")(l.reverse)(r.reverseIterator.toList)
      q("foreach")(l)({ val b = List.newBuilder[dd.T]; r.foreach(b += _); b.result() })
      q("sameElements")(true)(r.sameElements(l)); q("sameElements.tail")(n == 0)(r.sameElements(l.drop(1))); q("sameElements.it")(true)(r.sameElements(l.iterator))
      val lt = (t: dd.T) => dd.big(t) < x; val ne = (t: dd.T) => dd.big(t) != x
      for ((pn, pr) <- List("lt" -> lt, "ne" -> ne)) {
        q(s"takeWhile.$pn", dd.show(x))(l.takeWhile(pr))(r.takeWhile(pr)); q(s"dropWhile.$pn", dd.show(x))(l.dropWhile(pr))(r.dropWhile(pr))
        q(s"span.$pn", dd.show(x))(l.span(pr))(r.span(pr))
        if (!(quarantineKnown && dd.isRange && n == 0)) {
        q(s"indexWhere.$pn", s"${dd.show(x)},$i")(l.indexWhere(pr, i))(r.indexWhere(pr, i)); q(s"lastIndexWhere.$pn", s"${dd.show(x)},$i")(l.lastIndexWhere(pr, i))(r.lastIndexWhere(pr, i))
        q(s"segmentLength.$pn", s"${dd.show(x)},$i")(l.segmentLength(pr, i))(r.segmentLength(pr, i))
        }
      }
      val g = rnd.nextInt(8) match { case 0 => Int.MaxValue; case 1 => 0; case 2 => -1; case _ => 1 + rnd.nextInt(5) }; val st = rnd.nextInt(6) match { case 0 => Int.MaxValue; case _ => 1 + rnd.nextInt(4) }
      q("grouped", s"($g)")(l.grouped(g).toList)(r.grouped(g).map(_.toList).toList)
      q("sliding", s"($g,$st)")(l.sliding(g, st).toList)(r.sliding(g, st).map(_.toList).toList)
      if (n <= 40) { q("tails")(l.tails.toList)(r.tails.map(_.toList).toList); q("inits")(l.inits.toList)(r.inits.map(_.toList).toList) }
      if (!(quarantineKnown && v.m.step == dd.lo.orNull)) q("sorted")(l.sorted(ord))(r.sorted(ord)); q("sorted.rev")(l.sorted(ord.reverse))(r.sorted(ord.reverse)); q("distinct")(l)(r.distinct); q("map")(l)(r.map(identity)); q("indices")(l.indices.toList)(r.indices.toList)
      q("startsWith", s"$i")(l.startsWith(l.take(2), i))(r.startsWith(l.take(2), i)); q("endsWith")(true)(r.endsWith(l.takeRight(2)))
      q("patch", s"($i, _, $j)")(l.patch(i, l.take(1), j))(r.patch(i, l.take(1), j)); q("view.slice", s"($i,$j)")(l.slice(i, j))(r.view.slice(i, j).toList)
      q("diff")(l.drop(1))(r.diff(l.take(1))); q("intersect")(l.take(2))(r.intersect(l.take(2)))
      if (top && n > 0) {
        val m = v.m
        q("alt.equal")((true, true))({ val alt = dd.mk(T(m.start), T(v.el(c - 1)), T(m.step), true); (r == alt && alt == r, r.hashCode == alt.hashCode) })
      }
      if (top && n == 0) q("empty.equal")((true, true))({ val alt = dd.mk(T(dd.clamp(1)), T(dd.clamp(1)), T(dd.clamp(1)), false); (r == alt && alt == r, r.hashCode == alt.hashCode) })
    }
  }

  def oneCase(): Unit = {
    d = Dom.all(rnd.nextInt(Dom.all.size)); val dd: Dom = d
    val start = genVal()
    var step = rnd.nextInt(20) match {
      case 0 | 1 | 2 | 3 | 4 => BigInt(if (rnd.nextBoolean()) 1 else -1)
      case 5 | 6 | 7 | 8 => BigInt(rnd.nextInt(11) - 5)
      case 9 | 10 | 11 | 12 => genVal()
      case 13 => dd.anchors(rnd.nextInt(2))
      case _ => BigInt(0) // replaced below
    }
    var end = genVal()
    if (step == 0 && rnd.nextInt(10) > 0) step = (end - start) / (rnd.nextInt(6) + 1) + rnd.nextInt(3) - 1
    if (rnd.nextInt(5) < 2) end = start + step * rnd.nextInt(60) + rnd.nextInt(3) - 1
    step = dd.clamp(step); end = dd.clamp(end)
    val incl = rnd.nextBoolean()
    val m = M(start, end, step, incl)
    desc = s"${dd.name}(${dd.show(start)} ${if (incl) "to" else "until"} ${dd.show(end)} by ${dd.show(step)})"; chain = ""
    var r: IndexedSeq[dd.T] = null
    // a zero step is rejected at construction by Range and on first use by NumericRange
    if (step == 0) { q("step0.length")(throw new ME(IAE))(dd.mk(dd.from(start), dd.from(end), dd.from(step), incl).length); return }
    val built = q("construct")(())({ r = dd.mk(dd.from(start), dd.from(end), dd.from(step), incl); () })
    if (!built || r == null) return
    var v = View(m, 0, m.count, false)
    val ts = transformers(dd)
    var k = rnd.nextInt(3); var top = true
    while (k > 0) {
      k -= 1; top = false
      val (tn, tm, ta) = ts(rnd.nextInt(ts.size)); val i = idx(v.c); val j = idx(v.c)
      // slice (and the non-Range right-hand operations) may copy the result
      var nv: View = null; var nr: IndexedSeq[dd.T] = null
      val expV = try { nv = tm(v, i, j); null } catch { case e: ME => e.n }
      if (expV == "skip") return
      val cheap = dd.isRange || v.c <= 100000 || (!r.isInstanceOf[Vector[_]] && !Set("slice", "takeRight", "dropRight")(tn))
      if (!cheap) return
      val pre = chain
      chain = pre + s".$tn($i,$j)"
      // tail and init of an empty range throw NoSuchElementException from Range, UnsupportedOperationException elsewhere
      val ok = q("transform", "", v.big)(if (expV != null) "EX:empty" else ())(emptyEx(ev({ nr = ta(r, i, j); () })))
      if (!ok || nr == null || nv == null) { sigs.filterInPlace((k, _) => !k.contains("-> EX:skip")); return }
      v = nv; r = nr
      if (!dd.isRange && r.isInstanceOf[Vector[_]] && v.c > 100000) return
    }
    queries(dd)(r, v, top)
  }

  def main(args: Array[String]): Unit = {
    val seed = args(0).toLong; val secs = args(1).toInt
    args.drop(2).foreach(a => if (a.startsWith("skip=")) skip = a.drop(5).split(',').toSet else if (a == "quarantine=1") quarantineKnown = true)
    rnd = new Random(seed)
    val deadline = System.nanoTime() + secs * 1000000000L
    val worker = new Thread(() => { while (System.nanoTime() < deadline) { oneCase(); cases += 1 } })
    worker.setDaemon(true); beat = System.nanoTime(); worker.start()
    while (worker.isAlive) {
      worker.join(500)
      if (worker.isAlive && System.nanoTime() - beat > 8000000000L) { println(s"HANG (no result after 8s) at: $where"); report(); sys.exit(2) }
    }
    report()
  }
  def report(): Unit = {
    println(s"==== cases=$cases checks=$checks signatures=${sigs.size}")
    sigs.toList.sortBy(_._1).foreach { case (s, (n, ex)) => println(s"$n\t$s"); ex.foreach(e => println("\t\t" + e)) }
  }
}
