// Iterator combinators against List; LazyList evaluation counts against its Scaladoc tags ($initiallyLazy,
// $preservesLaziness) and memoisation; scala.util.Sorting and sorted/sortBy/sortInPlace against java.util.Arrays;
// BigInt / BigDecimal against java.math; scala.util.Random bounds; StringOps.format against String.format.
import scala.collection.{immutable => im, mutable => mu}
import scala.util.Random
import java.{math => jm}
import H._

final case class ICase(src: Int, xs: List[Int], chain: List[(Int, Int, Int)], term: Int, a: Int) {
  override def toString = s"src=$src xs=${xs.take(30).mkString(",")}${if (xs.size > 30) s",..(${xs.size})" else ""} chain=${chain.mkString(",")} term=$term a=$a"
}

class IteratorFam extends Fam {
  type P = ICase
  val name = "Iterator"
  def gen(r: Random) = { val n = r.nextInt(5) match { case 0 => r.nextInt(3); case 1 | 2 => r.nextInt(20); case _ => r.nextInt(300) }
    Vector(ICase(r.nextInt(10), List.fill(n)(r.nextInt(if (r.nextBoolean()) 6 else 1000)), List.fill(r.nextInt(4))((r.nextInt(22), r.nextInt(6), 1 + r.nextInt(5))), r.nextInt(24), r.nextInt(8))) }
  def source(c: ICase): Iterator[Int] = { val xs = c.xs; val n = xs.size; val arr = xs.toArray
    c.src match {
      case 0 => xs.iterator
      case 1 => Iterator.unfold(0)(i => if (i < n) Some((arr(i), i + 1)) else None)
      case 2 => Iterator.tabulate(n)(arr(_))
      case 3 => Iterator.iterate(0)(_ + 1).take(n).map(arr(_))
      case 4 => { var i = -1; Iterator.continually { i += 1; i }.takeWhile(_ < n).map(arr(_)) }
      case 5 => Iterator.from(0).take(n).map(arr(_))
      case 6 => xs.take(n / 2).iterator ++ xs.drop(n / 2).iterator
      case 7 => xs.iterator.buffered
      case 8 => Iterator.range(0, n).map(arr(_))
      case _ => xs.toVector.iterator.drop(0)
    }
  }
  def step(it: Iterator[Int], l: List[Int], op: (Int, Int, Int)): (Iterator[Int], List[Int]) = { val (o, a, k) = op
    o match {
      case 0 => (it.map(_ * 2 + 1), l.map(_ * 2 + 1)); case 1 => (it.filter(_ % 3 != a % 3), l.filter(_ % 3 != a % 3)); case 2 => (it.take(a + k), l.take(a + k))
      case 3 => (it.drop(a), l.drop(a)); case 4 => (it.slice(a, a + k * 3), l.slice(a, a + k * 3)); case 5 => (it.takeWhile(_ % 5 != a), l.takeWhile(_ % 5 != a))
      case 6 => (it.dropWhile(_ % 5 != a), l.dropWhile(_ % 5 != a)); case 7 => (it.flatMap(x => List.fill(x % 3)(x)), l.flatMap(x => List.fill(x % 3)(x)))
      case 8 => (it.collect { case x if x % 2 == 0 => x / 2 }, l.collect { case x if x % 2 == 0 => x / 2 }); case 9 => (it.scanLeft(a)(_ + _), l.scanLeft(a)(_ + _))
      case 10 => (it.distinct, l.distinct); case 11 => (it.distinctBy(_ % 7), l.distinctBy(_ % 7)); case 12 => (it.grouped(k).map(_.sum), l.grouped(k).map(_.sum).toList)
      case 13 => (it.sliding(k, a + 1).map(_.sum), l.sliding(k, a + 1).map(_.sum).toList); case 14 => (it.padTo(a * 3, -1), l.padTo(a * 3, -1))
      case 15 => (it.patch(a, Iterator(7, 8), k), l.patch(a, List(7, 8), k)); case 16 => (it.zipWithIndex.map(p => p._1 * 1000 + p._2), l.zipWithIndex.map(p => p._1 * 1000 + p._2))
      case 17 => (it.zip(Iterator.from(a)).map(p => p._1 + p._2), l.zip(LazyList.from(a)).map(p => p._1 + p._2)); case 18 => (it.zipAll(List.fill(k * 4)(9), -1, -2).map(p => p._1 * 10 + p._2), l.zipAll(List.fill(k * 4)(9), -1, -2).map(p => p._1 * 10 + p._2))
      case 19 => (it.concat(Iterator(a, k)), l ++ List(a, k)); case 20 => (it.withFilter(_ > a).map(identity), l.filter(_ > a))
      case _ => (it.sliding(k).withPartial(false).map(_.sum), l.sliding(k).filter(_.size == k).map(_.sum).toList.pipeIf(l.size < k && l.nonEmpty))
    }
  }
  implicit class PipeIf(l: List[Int]) { def pipeIf(b: Boolean) = if (b) Nil else l }
  def run(prog: Vector[ICase]): Unit = prog.foreach { c =>
    val (it, l) = c.chain.foldLeft((source(c), c.xs)) { case ((i, m), op) => step(i, m, op) }
    val a = c.a; val ch = c.chain.map(_._1).mkString("-")
    def t(nm: String)(e: => Any)(x: => Any) = chk(name, nm)(e)(x)
    c.term match {
      case 0 => t("toList")(l)(it.toList)
      case 1 => val (p, q) = it.span(_ % 4 != a % 4); val (lp, lq) = l.span(_ % 4 != a % 4); t("span(first,second)")((lp, lq))((p.toList, q.toList))
      case 2 => val (p, q) = it.span(_ % 4 != a % 4); val (lp, lq) = l.span(_ % 4 != a % 4); t("span(second,first)")((lq, lp))((q.toList, p.toList))
      case 3 => val (p, q) = it.partition(_ % 3 == 0); t("partition(interleaved)")((l.filter(_ % 3 == 0), l.filterNot(_ % 3 == 0)))({ val b1 = mu.ListBuffer.empty[Int]; val b2 = mu.ListBuffer.empty[Int]
        while (p.hasNext || q.hasNext) { if (q.hasNext) b2 += q.next(); if (p.hasNext) b1 += p.next() }; (b1.toList, b2.toList) })
      case 4 => val (p, q) = it.duplicate; t("duplicate")((l, l))({ val b1 = mu.ListBuffer.empty[Int]; val b2 = mu.ListBuffer.empty[Int]
        var turn = 0; while (p.hasNext || q.hasNext) { turn += 1; if ((turn / (a + 1)) % 2 == 0 && p.hasNext) b1 += p.next() else if (q.hasNext) b2 += q.next() else b1 += p.next() }; (b1.toList, b2.toList) })
      case 5 => t("indexWhere/indexOf")(l.indexWhere(_ > 500, a))(it.indexWhere(_ > 500, a))
      case 6 => t("sameElements")(true)(it.sameElements(l))
      case 7 => t("corresponds")(true)(it.corresponds(l)(_ == _))
      case 8 => val ks = it.knownSize; val s = it.size; check(name, "knownSize", ks == -1 || ks == s, s"knownSize $ks size $s"); t("size")(l.size)(s)
      case 9 => val b = it.buffered; t("buffered")((l.headOption, l.headOption, l))((b.headOption, b.headOption, b.toList))
      case 10 => t("nextOption")((l.headOption, l.drop(1).headOption))((it.nextOption(), it.nextOption()))
      case 11 => t("find/exists")((l.find(_ > a * 100), l.exists(_ == a)))({ val (p, q) = it.duplicate; (p.find(_ > a * 100), q.exists(_ == a)) })
      case 12 => t("foldLeft/sum")((l.foldLeft(0L)(_ * 31 + _)))(it.foldLeft(0L)(_ * 31 + _))
      case 13 => t("max/minOption")((l.maxOption, l.minOption))({ val (p, q) = it.duplicate; (p.maxOption, q.minOption) })
      case 14 => t("mkString")(l.mkString("<", ",", ">"))(it.mkString("<", ",", ">"))
      case 15 => val arr = Array.fill(12)(-9); val e = Array.fill(12)(-9); val n = l.copyToArray(e, a, 5); t("copyToArray")((n, e.toList))((it.copyToArray(arr, a, 5), arr.toList))
      case 16 => t("grouped.withPadding")(l.grouped(a + 1).map(g => g.padTo(a + 1, 0)).toList)(it.grouped(a + 1).withPadding(0).map(_.toList).toList)
      case 17 => t("sliding.withPartial")(if (l.isEmpty) Nil else l.sliding(a + 1, 2).toList.filter(_.nonEmpty))(it.sliding(a + 1, 2).map(_.toList).toList)
      case 18 => t("drop-then-take")(l.drop(a).take(3))(it.drop(a).take(3).toList)
      case 19 => val nx = mu.ListBuffer.empty[Int]; t("next-then-rest")((l.take(2), l.drop(2)))({ if (it.hasNext) nx += it.next(); if (it.hasNext) nx += it.next(); (nx.toList, it.toList) })
      case 20 => t("toSeq/toVector/toSet")((l.toVector, l.toSet))({ val v = it.toVector; (v, v.toSet) })
      case 21 => t("tapEach count")((l.size, l))({ var n = 0; val r = it.tapEach(_ => n += 1).toList; (n, r) })
      case 22 => t("splitAt via take/drop on duplicate")((l.take(a), l.drop(a)))({ val (p, q) = it.duplicate; (p.take(a).toList, q.drop(a).toList) })
      case _ => t("reduceLeftOption/scanLeft")((l.reduceLeftOption(_ - _), l.scanLeft(0)(_ - _)))({ val (p, q) = it.duplicate; (p.reduceLeftOption(_ - _), q.scanLeft(0)(_ - _).toList) })
    }
  }
}

final case class LCase(n: Int, op: Int, a: Int, k: Int, term: Int) { override def toString = s"n=$n op=${LazyFam.ops(op)._1} a=$a k=$k term=$term" }
object LazyFam {
  // (name, Scaladoc tag: IL initially lazy / PL preserves laziness, op on LazyList, same op on List, same op on Iterator or null)
  type LOp = (String, String, (LazyList[Int], Int, Int) => LazyList[Int], (List[Int], Int, Int) => List[Int], (Iterator[Int], Int, Int) => Iterator[Int])
  val ops: Vector[LOp] = Vector(
    ("map", "PL", (l, a, k) => l.map(_ + a), (l, a, k) => l.map(_ + a), (l, a, k) => l.map(_ + a)),
    ("filter", "PL", (l, a, k) => l.filter(_ % 3 != a % 3), (l, a, k) => l.filter(_ % 3 != a % 3), (l, a, k) => l.filter(_ % 3 != a % 3)),
    ("filterNot", "PL", (l, a, k) => l.filterNot(_ % 3 != a % 3), (l, a, k) => l.filterNot(_ % 3 != a % 3), (l, a, k) => l.filterNot(_ % 3 != a % 3)),
    ("collect", "PL", (l, a, k) => l.collect { case x if x % 2 == 0 => x + a }, (l, a, k) => l.collect { case x if x % 2 == 0 => x + a }, (l, a, k) => l.collect { case x if x % 2 == 0 => x + a }),
    ("flatMap", "PL", (l, a, k) => l.flatMap(x => List.fill(x % 3)(x)), (l, a, k) => l.flatMap(x => List.fill(x % 3)(x)), (l, a, k) => l.flatMap(x => List.fill(x % 3)(x))),
    ("take", "PL", (l, a, k) => l.take(a), (l, a, k) => l.take(a), (l, a, k) => l.take(a)),
    ("drop", "IL", (l, a, k) => l.drop(a), (l, a, k) => l.drop(a), null),
    ("dropRight", "IL", (l, a, k) => l.dropRight(a), (l, a, k) => l.dropRight(a), null),
    ("takeRight", "IL", (l, a, k) => l.takeRight(a), (l, a, k) => l.takeRight(a), null),
    ("slice", "IL", (l, a, k) => l.slice(a, a + k), (l, a, k) => l.slice(a, a + k), null),
    ("dropWhile", "IL", (l, a, k) => l.dropWhile(_ % 5 != a % 5), (l, a, k) => l.dropWhile(_ % 5 != a % 5), null),
    ("takeWhile", "PL", (l, a, k) => l.takeWhile(_ % 5 != a % 5), (l, a, k) => l.takeWhile(_ % 5 != a % 5), (l, a, k) => l.takeWhile(_ % 5 != a % 5)),
    ("zip", "PL", (l, a, k) => l.zip(LazyList.from(a)).map(p => p._1 * 7 + p._2), (l, a, k) => l.zip(LazyList.from(a)).map(p => p._1 * 7 + p._2), (l, a, k) => l.zip(Iterator.from(a)).map(p => p._1 * 7 + p._2)),
    ("zipWithIndex", "PL", (l, a, k) => l.zipWithIndex.map(p => p._1 - p._2), (l, a, k) => l.zipWithIndex.map(p => p._1 - p._2), (l, a, k) => l.zipWithIndex.map(p => p._1 - p._2)),
    ("zipAll", "PL", (l, a, k) => l.zipAll(LazyList.fill(a)(5), 0, 1).map(p => p._1 * 3 + p._2), (l, a, k) => l.zipAll(List.fill(a)(5), 0, 1).map(p => p._1 * 3 + p._2), null),
    ("scanLeft", "PL", (l, a, k) => l.scanLeft(a)(_ + _), (l, a, k) => l.scanLeft(a)(_ + _), (l, a, k) => l.scanLeft(a)(_ + _)),
    ("padTo", "PL", (l, a, k) => l.padTo(a * 2, -1), (l, a, k) => l.padTo(a * 2, -1), (l, a, k) => l.padTo(a * 2, -1)),
    ("patch", "PL", (l, a, k) => l.patch(a, List(-5, -6), k), (l, a, k) => l.patch(a, List(-5, -6), k), (l, a, k) => l.patch(a, Iterator(-5, -6), k)),
    ("updated", "PL", (l, a, k) => l.updated(a, -7), (l, a, k) => if (a < l.size) l.updated(a, -7) else throw new IndexOutOfBoundsException, null),
    ("appended", "PL", (l, a, k) => l.appended(a), (l, a, k) => l.appended(a), (l, a, k) => l ++ Iterator(a)),
    ("prepended", "PL", (l, a, k) => l.prepended(a), (l, a, k) => l.prepended(a), null),
    ("appendedAll", "PL", (l, a, k) => l.appendedAll(LazyList.range(0, k)), (l, a, k) => l.appendedAll(List.range(0, k)), (l, a, k) => l ++ Iterator.range(0, k)),
    ("lazyAppendedAll", "PL", (l, a, k) => l.lazyAppendedAll(LazyList.range(0, k)), (l, a, k) => l.appendedAll(List.range(0, k)), (l, a, k) => l ++ Iterator.range(0, k)),
    ("prependedAll", "PL", (l, a, k) => l.prependedAll(List.range(0, k)), (l, a, k) => l.prependedAll(List.range(0, k)), null),
    ("tapEach", "PL", (l, a, k) => l.tapEach(_ => ()), (l, a, k) => l, (l, a, k) => l.tapEach(_ => ())),
    ("diff", "PL", (l, a, k) => l.diff(List(a, k, a)), (l, a, k) => l.diff(List(a, k, a)), null),
    ("intersect", "PL", (l, a, k) => l.intersect(List(a, k, 3)), (l, a, k) => l.intersect(List(a, k, 3)), null),
    ("partition._1", "PL", (l, a, k) => l.partition(_ % 2 == 0)._1, (l, a, k) => l.partition(_ % 2 == 0)._1, (l, a, k) => l.partition(_ % 2 == 0)._1),
    ("partitionMap._2", "PL", (l, a, k) => l.partitionMap(x => if (x % 2 == 0) Left(x) else Right(-x))._2, (l, a, k) => l.partitionMap(x => if (x % 2 == 0) Left(x) else Right(-x))._2, null),
    ("unzip._2", "PL", (l, a, k) => l.map(x => (x, x * 2)).unzip._2, (l, a, k) => l.map(x => x * 2), null),
    ("distinct", "PL?", (l, a, k) => l.distinct, (l, a, k) => l.distinct, (l, a, k) => l.distinct),
    ("tail", "IL?", (l, a, k) => l.tail, (l, a, k) => l.tail, null),
    ("grouped.map", "PL?", (l, a, k) => LazyList.from(l.grouped(k + 1).map(_.sum)), (l, a, k) => l.grouped(k + 1).map(_.sum).toList, null)
  )
}

class LazyFam extends Fam {
  type P = LCase
  val name = "LazyList"
  def gen(r: Random) = { val n = r.nextInt(4) match { case 0 => r.nextInt(3); case _ => r.nextInt(40) }
    Vector(LCase(n, r.nextInt(LazyFam.ops.size), r.nextInt(12), r.nextInt(6), r.nextInt(6))) }
  def run(prog: Vector[LCase]): Unit = prog.foreach { c =>
    val (nm, tag, f, g, h) = LazyFam.ops(c.op); val n = c.n
    val elems = new Array[Int](n); var cells = 0
    def src(i: Int): LazyList[Int] = { cells += 1; if (i >= n) LazyList.empty else { elems(i) += 0; ({ elems(i) += 1; i * 37 % 101 } #:: src(i + 1)) } }
    val model = List.tabulate(n)(_ * 37 % 101)
    val ll = LazyList.empty[Int] #::: src(0) // nothing is evaluated yet
    check(name, s"$nm/source lazy", cells == 0 && elems.sum == 0, s"cells $cells")
    if ((nm == "tail" && n == 0) || (nm == "updated" && c.a >= n)) { Excl.hit("LazyList.updated/tail out of range: exception type and when it is thrown are unspecified"); return }
    val res = f(ll, c.a, c.k)
    if (tag == "IL") check(name, s"$nm/initiallyLazy", cells == 0 && elems.sum == 0, s"after building: cells $cells elements ${elems.toList}")
    if (tag == "PL") check(name, s"$nm/preservesLaziness:no element on construction", elems.sum == 0, s"after building: elements ${elems.toList}")
    val exp = ev(g(model, c.a, c.k))
    c.term match {
      case 0 => chk(name, s"$nm/toList")(exp)(res.toList)
      case 1 => // a prefix: evaluate no more source elements than an Iterator would
        val k = c.k; chk(name, s"$nm/take(k).toList")(ev(g(model, c.a, c.k)) match { case l: List[_] => l.take(k); case x => x })(res.take(k).toList)
        if (h != null) { val ie = new Array[Int](n); val it = h(Iterator.tabulate(n) { i => ie(i) += 1; i * 37 % 101 }, c.a, c.k); it.take(k).toList
          check(name, s"$nm/take(k): elements evaluated <= Iterator's", elems.sum <= ie.sum, s"LazyList evaluated ${elems.toList} Iterator ${ie.toList}") }
      case 2 => chk(name, s"$nm/headOption")(exp match { case l: List[_] => l.headOption; case x => x })(res.headOption)
      case 3 => chk(name, s"$nm/length")(exp match { case l: List[_] => l.length; case x => x })(res.length)

      case 4 => val i = c.a; chk(name, s"$nm/apply")(exp match { case l: List[_] => l(i); case x => x })(res(i))
        if (Set("map", "zip", "zipWithIndex", "updated", "tapEach", "drop", "slice", "unzip._2", "take")(nm) && i < n) check(name, s"$nm/apply evaluates a prefix up to the index", elems.sum <= i + 2 + (if (nm == "drop" || nm == "slice") c.a else 0), s"elements ${elems.toList}")
      case _ => chk(name, s"$nm/twice")((exp, exp))((res.toList, res.toList))
    }
    check(name, s"$nm/memoised", elems.forall(_ <= 1), s"elements evaluated more than once: ${elems.toList}")
    // Scaladoc: elements are computed in order and are never skipped
    check(name, s"$nm/computed in order, never skipped", elems.toList == elems.toList.sorted(Ordering.Int.reverse), s"elements ${elems.toList}")
  }
}

final case class SortCase(t: Int, xs: Vector[Int], how: Int) { override def toString = s"type=$t how=$how xs=${xs.take(40).mkString(",")}${if (xs.size > 40) s",..(${xs.size})" else ""}" }
class SortFam extends Fam {
  type P = SortCase
  val name = "Sorting"
  def gen(r: Random) = { val n = r.nextInt(4) match { case 0 => r.nextInt(10); case 1 => r.nextInt(50); case 2 => r.nextInt(500); case _ => r.nextInt(5000) }
    val v = if (r.nextBoolean()) 1 + r.nextInt(5) else 1 + r.nextInt(100000)
    val mode = r.nextInt(4); Vector(SortCase(r.nextInt(9), Vector.tabulate(n)(i => mode match { case 0 => r.nextInt(v); case 1 => i; case 2 => n - i; case _ => if (r.nextInt(8) == 0) r.nextInt(v) else i }), r.nextInt(6))) }
  def run(prog: Vector[SortCase]): Unit = prog.foreach { c =>
    val xs = c.xs
    def dbl(x: Int) = x % 11 match { case 0 => Double.NaN; case 1 => -0.0; case 2 => 0.0; case 3 => Double.NegativeInfinity; case _ => x / 7.0 - 50 }
    def go[T: scala.reflect.ClassTag](tn: String, a: Array[T], jsort: Array[T] => Unit)(implicit ord: Ordering[T]): Unit = {
      val exp = a.clone; jsort(exp); val e = exp.toList.map(norm)
      c.how match {
        case 0 => chk(name, s"$tn quickSort")(e)({ val b = a.clone; scala.util.Sorting.quickSort(b); b.toList.map(norm) })
        case 1 => chk(name, s"$tn stableSort")(e)({ val b = a.clone; scala.util.Sorting.stableSort(b); b.toList.map(norm) })
        case 2 => chk(name, s"$tn sortInPlace")(e)({ val b = a.clone; b.sortInPlace(); b.toList.map(norm) })
        case 3 => chk(name, s"$tn sorted")((e, e, e))((a.sorted.toList.map(norm), a.toList.sorted.map(norm), a.toVector.sorted.toList.map(norm)))
        case 4 => chk(name, s"$tn sorted(reverse)")(e.reverse)(a.sorted(ord.reverse).toList.map(norm).pipe(identity))
        case _ => chk(name, s"$tn ArrayBuffer.sortInPlace")(e)(mu.ArrayBuffer.from(a).sortInPlace().toList.map(norm))
      }
    }
    c.t match {
      case 0 => go[Int]("Int", xs.toArray, java.util.Arrays.sort(_))
      case 1 => go[Long]("Long", xs.map(_.toLong * 1000003L).toArray, java.util.Arrays.sort(_))
      case 2 => go[Double]("Double", xs.map(dbl).toArray, java.util.Arrays.sort(_))(implicitly, Ordering.Double.TotalOrdering)
      case 3 => go[Float]("Float", xs.map(x => dbl(x).toFloat).toArray, java.util.Arrays.sort(_))(implicitly, Ordering.Float.TotalOrdering)
      case 4 => go[Char]("Char", xs.map(_.toChar).toArray, java.util.Arrays.sort(_))
      case 5 => go[Short]("Short", xs.map(_.toShort).toArray, java.util.Arrays.sort(_))
      case 6 => go[Byte]("Byte", xs.map(_.toByte).toArray, java.util.Arrays.sort(_))
      case 7 => go[String]("String", xs.map(x => "s" + (x % 1000)).toArray, a => java.util.Arrays.sort(a.asInstanceOf[Array[Object]]))
      case _ => // stability: sort (key, index) by key only
        val ps = xs.zipWithIndex.map { case (x, i) => (x % 13, i) }.toArray; val ordK = Ordering.by[(Int, Int), Int](_._1)
        val exp = ps.clone; java.util.Arrays.sort(exp, ordK); val e = exp.toList
        chk(name, s"stable how=${c.how}")((e, e, e, e, e, e))({ val b = ps.clone; scala.util.Sorting.stableSort(b)(ordK); val b2 = ps.clone; b2.sortInPlaceBy(_._1)
          (b.toList, b2.toList, ps.sortBy(_._1).toList, ps.toList.sortBy(_._1), ps.toVector.sortWith(_._1 < _._1).toList, mu.ArrayBuffer.from(ps).sortInPlaceBy(_._1).toList) })
    }
  }
  implicit class Pipe[A](a: A) { def pipe[B](f: A => B) = f(a) }
}

final case class BCase(op: Int, x: String, y: String, z: Int) { override def toString = s"op=$op x=$x y=$y z=$z" }
class BigFam extends Fam {
  type P = BCase
  val name = "BigInt/BigDecimal"
  def num(r: Random): String = { val neg = if (r.nextInt(3) == 0) "-" else ""
    r.nextInt(7) match {
      case 0 => neg + r.nextInt(10); case 1 => neg + r.nextInt(1000000); case 2 => neg + (r.nextLong() >>> r.nextInt(64)).abs
      case 3 => neg + BigInt(r.nextInt(130) + 1, r); case 4 => (BigInt(Long.MaxValue) + r.nextInt(5) - 2).toString; case 5 => (BigInt(Long.MinValue) + r.nextInt(5) - 2).toString
      case _ => neg + (BigInt(2).pow(r.nextInt(80)) + r.nextInt(3) - 1) }
  }
  def gen(r: Random) = Vector(BCase(r.nextInt(40), num(r), num(r), r.nextInt(70)))
  def run(prog: Vector[BCase]): Unit = prog.foreach { c =>
    val a = BigInt(c.x); val b = BigInt(c.y); val ja = new jm.BigInteger(c.x); val jb = new jm.BigInteger(c.y); val z = c.z
    def t(n: String)(e: => Any)(x: => Any) = chk(name, s"BigInt $n")(ev(e) match { case bi: jm.BigInteger => bi.toString; case o => o })(ev(x) match { case bi: BigInt => bi.toString; case o => o })
    c.op match {
      case 0 => t("+")(ja.add(jb))(a + b); case 1 => t("-")(ja.subtract(jb))(a - b); case 2 => t("*")(ja.multiply(jb))(a * b)
      case 3 => t("/")(ja.divide(jb))(a / b); case 4 => t("%")(ja.remainder(jb))(a % b); case 5 => t("/%")(ja.divideAndRemainder(jb).toList.map(_.toString))({ val (q, r) = a /% b; List(q.toString, r.toString) })
      case 6 => t("mod")(ja.mod(jb))(a mod b); case 7 => t("gcd")(ja.gcd(jb))(a gcd b); case 8 => t("pow")(ja.pow(z % 9))(a.pow(z % 9))
      case 9 => t("<<,>>")((ja.shiftLeft(z).toString, ja.shiftRight(z).toString))(((a << z).toString, (a >> z).toString))
      case 10 => t("&,|,^,&~")((ja.and(jb).toString, ja.or(jb).toString, ja.xor(jb).toString, ja.andNot(jb).toString))(((a & b).toString, (a | b).toString, (a ^ b).toString, (a &~ b).toString))
      case 11 => t("bits")((ja.testBit(z), ja.setBit(z).toString, ja.clearBit(z).toString, ja.flipBit(z).toString, ja.bitLength, ja.bitCount, ja.getLowestSetBit))((a.testBit(z), a.setBit(z).toString, a.clearBit(z).toString, a.flipBit(z).toString, a.bitLength, a.bitCount, a.lowestSetBit))
      case 12 => t("compare/equals")((ja.compareTo(jb).sign, ja.equals(jb), ja.min(jb).toString, ja.max(jb).toString))((a.compare(b).sign, a == b, (a min b).toString, (a max b).toString))
      case 13 => t("conversions")((ja.intValue, ja.longValue, ja.doubleValue, ja.floatValue, ja.shortValue, ja.byteValue))((a.toInt, a.toLong, a.toDouble, a.toFloat, a.toShort, a.toByte))
      case 14 => val bl = ja.bitLength; t("isValid")((bl <= 31, bl <= 63, bl <= 15, bl <= 7, ja.compareTo(jm.BigInteger.valueOf(Char.MaxValue)) <= 0 && ja.signum >= 0))((a.isValidInt, a.isValidLong, a.isValidShort, a.isValidByte, a.isValidChar))
      case 15 => t("hash cooperative")(true)((!a.isValidLong || a.## == a.toLong.##) && (!a.isValidInt || (a == a.toInt && a.toInt == a)) && (a.isValidLong || a.## == a.##))
      case 16 => t("equals Double")((ja.doubleValue == ja.doubleValue && new jm.BigDecimal(ja).compareTo(new jm.BigDecimal(ja.doubleValue)) == 0))(a == a.toDouble)
      case 17 => t("abs/signum/neg")((ja.abs.toString, ja.signum, ja.negate.toString))((a.abs.toString, a.signum, (-a).toString))
      case 18 => t("toString(radix)")((ja.toString(2 + z % 35), new jm.BigInteger(ja.toString(2 + z % 35), 2 + z % 35).toString))((a.toString(2 + z % 35), BigInt(a.toString(2 + z % 35), 2 + z % 35).toString))
      case 19 => if (jb.signum > 0) t("modPow/modInverse")((ja.modPow(jm.BigInteger.valueOf(z), jb).toString, util.Try(ja.modInverse(jb).toString).toOption))((a.modPow(z, b).toString, util.Try(a.modInverse(b).toString).toOption))
      case 20 => t("isWhole/isValidDouble")((true, ja.bitLength < 53 || ja.getLowestSetBit >= ja.bitLength - 53))((a.isWhole, a.isValidDouble))
      case 21 => t("isProbablePrime")(ja.abs.isProbablePrime(20))(a.abs.isProbablePrime(20))
      case 22 => t("toByteArray/apply")((ja.toByteArray.toList, ja.toString))((a.toByteArray.toList, BigInt(a.toByteArray).toString))
      case 23 => t("long ops")({ val l = jb.longValue; (ja.add(jm.BigInteger.valueOf(l)).toString, ja.multiply(jm.BigInteger.valueOf(l)).toString, ja.compareTo(jm.BigInteger.valueOf(l)).sign) })({ val l = b.toLong; ((a + l).toString, (a * l).toString, a.compare(BigInt(l)).sign) })
      case op => // BigDecimal: ordinary decimal values against java.math.BigDecimal in DECIMAL128
        val sx = c.x + (if (z % 3 == 0) "" else "." + (z * 7919 % 10000)); val sy = c.y + (if (z % 2 == 0) "" else "e" + (z % 9 - 4))
        val da = BigDecimal(sx); val db = BigDecimal(sy); val ka = new jm.BigDecimal(sx); val kb = new jm.BigDecimal(sy)
        // scala.math.BigDecimal(String) keeps the exact value, with a MathContext of at least its own precision (BigDecimal.exact)
        val mc = if (ka.precision <= 34) jm.MathContext.DECIMAL128 else new jm.MathContext(ka.precision, jm.RoundingMode.HALF_EVEN)
        def u(n: String)(e: => Any)(x: => Any) = chk(name, s"BigDecimal $n")(ev(e) match { case d: jm.BigDecimal => d.toString; case o => o })(ev(x) match { case d: BigDecimal => d.bigDecimal.toString; case o => o })
        op match {
          case 24 => u("+")(ka.add(kb, mc))(da + db); case 25 => u("-")(ka.subtract(kb, mc))(da - db); case 26 => u("*")(ka.multiply(kb, mc))(da * db)
          case 27 => u("/")(ka.divide(kb, mc))(da / db); case 28 => u("quot/rem")((ka.divideToIntegralValue(kb, mc).toString, ka.remainder(kb, mc).toString))(((da quot db).bigDecimal.toString, (da remainder db).bigDecimal.toString))
          case 29 => u("compare/equals")((ka.compareTo(kb).sign, ka.compareTo(kb) == 0))((da.compare(db).sign, da == db))
          case 30 => u("hash consistent with ==")(true)({ val e = BigDecimal(new jm.BigDecimal(sx).setScale(new jm.BigDecimal(sx).scale + 2)); da == e && da.## == e.## && (!da.isWhole || !da.isValidLong || (da.## == da.toLong.## && da == da.toLong)) })
          case 31 => u("setScale/rounding")((ka.setScale(z % 6, jm.RoundingMode.HALF_UP).toString, ka.setScale(z % 6, jm.RoundingMode.HALF_EVEN).toString, ka.round(new jm.MathContext(1 + z % 10)).toString))(
            (da.setScale(z % 6, BigDecimal.RoundingMode.HALF_UP).bigDecimal.toString, da.setScale(z % 6, BigDecimal.RoundingMode.HALF_EVEN).bigDecimal.toString, da.round(new jm.MathContext(1 + z % 10)).bigDecimal.toString))
          case 32 => u("conversions")((ka.doubleValue, ka.longValue, ka.intValue, ka.toBigInteger.toString, util.Try(ka.longValueExact).toOption, util.Try(ka.intValueExact).toOption))((da.toDouble, da.toLong, da.toInt, da.toBigInt.toString, da.toLongExact.pipeTry, da.toIntExact.pipeTry))
          case 33 => u("isValid/isWhole")((util.Try(ka.longValueExact).isSuccess, util.Try(ka.intValueExact).isSuccess, ka.stripTrailingZeros.scale <= 0))((da.isValidLong, da.isValidInt, da.isWhole))
          case 34 => u("pow/abs/signum/precision/ulp")((ka.pow(z % 6, mc).toString, ka.abs.toString, ka.signum, ka.precision, ka.ulp.toString))((da.pow(z % 6).bigDecimal.toString, da.abs.bigDecimal.toString, da.signum, da.precision, da.ulp.bigDecimal.toString))
          case 35 => val d = sx.toDouble; u("decimal(Double)/apply(Double)")((new jm.BigDecimal(java.lang.Double.toString(d), mc).toString, new jm.BigDecimal(java.lang.Double.toString(d), mc).toString))((BigDecimal.decimal(d).bigDecimal.toString, BigDecimal(d).bigDecimal.toString))
          case 36 => u("min/max/unary")((ka.min(kb).toString, ka.max(kb).toString, ka.negate.toString))(((da min db).bigDecimal.toString, (da max db).bigDecimal.toString, (-da).bigDecimal.toString))
          case 37 => u("equals Double/Long")((!ka.doubleValue.isInfinite && ka.compareTo(new jm.BigDecimal(java.lang.Double.toString(ka.doubleValue))) == 0, util.Try(ka.longValueExact).isSuccess))((da == da.toDouble, da == da.toLong))
          case 38 => u("toString/apply(String)")(ka.toString)(BigDecimal(da.toString).bigDecimal.toString)
          case _ => u("% and /%")((ka.remainder(kb, mc).toString, ka.divideToIntegralValue(kb, mc).toString))({ val (q, r) = da /% db; ((da % db).bigDecimal.toString, q.bigDecimal.toString) })
        }
    }
  }
  implicit class PT[A](a: => A) { def pipeTry: Option[A] = util.Try(a).toOption }
}

final case class RCase(seed: Long, op: Int, a: Int, b: Int) { override def toString = s"seed=$seed op=$op a=$a b=$b" }
class RandomFam extends Fam {
  type P = RCase
  val name = "Random/format"
  def gen(r: Random) = Vector(RCase(r.nextLong(), r.nextInt(12), r.nextInt(2000) - 1000, r.nextInt(2000) - 1000))
  def run(prog: Vector[RCase]): Unit = prog.foreach { c =>
    val r = new Random(c.seed); val lo = c.a min c.b; val hi = (c.a max c.b) + 1
    c.op match {
      case 0 => val x = r.between(lo, hi); check(name, "between(Int)", x >= lo && x < hi, s"$x not in [$lo,$hi)")
      case 1 => val x = r.between(lo.toLong * 1000000007L, hi.toLong * 1000000007L); check(name, "between(Long)", x >= lo.toLong * 1000000007L && x < hi.toLong * 1000000007L, s"$x")
      case 2 => val x = r.between(lo / 7.0, hi / 7.0); check(name, "between(Double)", x >= lo / 7.0 && x < hi / 7.0, s"$x not in [${lo / 7.0},${hi / 7.0})")
      case 3 => val x = r.between(lo / 7.0f, hi / 7.0f); check(name, "between(Float)", x >= lo / 7.0f && x < hi / 7.0f, s"$x not in [${lo / 7.0f},${hi / 7.0f})")
      case 4 => val xs = List.range(0, c.a.abs % 300); chk(name, "shuffle permutes")((xs, xs, xs.toVector))((r.shuffle(xs).sorted, r.shuffle(xs.toSet).toList.sorted, r.shuffle(mu.ArrayBuffer.from(xs)).sorted.toVector))
      case 5 => val s = r.alphanumeric.take(c.a.abs % 50).mkString; check(name, "alphanumeric", s.length == c.a.abs % 50 && s.forall(ch => ch.isLetterOrDigit && ch < 128), s)
      case 6 => val n = c.a.abs % 40; val s = r.nextString(n); check(name, "nextString length", s.length == n, s"${s.length}"); val k = r.nextInt(hi - lo); check(name, "nextInt(n)", k >= 0 && k < hi - lo, s"$k")
      case 7 => val fmt = List("%d", "%5d", "%-6d|", "%x", "%08X", "%,d", "%+d", "%o")(c.b.abs % 8); val v = c.a.toLong * 1234567L
        chk(name, s"format $fmt Long/BigInt")((String.format(fmt, java.lang.Long.valueOf(v)), String.format(fmt, java.math.BigInteger.valueOf(v))))((fmt.format(v), fmt.format(BigInt(v))))
      case 8 => val fmt = List("%.3f", "%10.2f", "%e", "%.0f", "%g", "%,.2f", "%s", "%-12.4e|")(c.b.abs % 8); val v = c.a / 7.0
        chk(name, s"format $fmt Double/BigDecimal")((String.format(fmt, java.lang.Double.valueOf(v)), String.format(fmt, new java.math.BigDecimal(c.a).divide(new java.math.BigDecimal(7), java.math.MathContext.DECIMAL64))))((fmt.format(v), fmt.format(BigDecimal(c.a) / BigDecimal(7, java.math.MathContext.DECIMAL64) match { case x => BigDecimal(x.bigDecimal.round(java.math.MathContext.DECIMAL64)) })))
      case 9 => val loc = List(java.util.Locale.ROOT, java.util.Locale.GERMANY, java.util.Locale.FRANCE)(c.b.abs % 3); val v = c.a * 1234.5678
        chk(name, "formatLocal")(String.format(loc, "%,.2f %s %d", java.lang.Double.valueOf(v), "x", java.lang.Integer.valueOf(c.a)))("%,.2f %s %d".formatLocal(loc, v, "x", c.a))
      case 10 => val s = "ab|cd||e" * (c.a.abs % 4); val v = s.view
        chk(name, "StringView")((s.slice(2, 9), s.map(_.toUpper), s.reverse, s.indexOf('|'), s.length, s.take(5) + s.drop(5)))((v.slice(2, 9).mkString, v.map(_.toUpper).mkString, v.reverse.mkString, v.indexOf('|'), v.length, (v.take(5) ++ v.drop(5)).mkString))
      case _ => val s = (0 until c.a.abs % 6).map(i => s"  |line$i\r\n").mkString + "end"
        chk(name, "lines/stripMargin")((s.split("\r\n|\n", -1).toList, s.split("\r\n|\n", -1).map(_.replaceFirst("^\\s*\\|", "")).mkString("\r\n")))((s.linesIterator.toList ++ (if (s.endsWith("\n")) List("") else Nil), s.stripMargin))
    }
  }
}

object MiscFams { def all: Seq[Fam] = Seq(new IteratorFam, new LazyFam, new SortFam, new BigFam, new RandomFam) }
