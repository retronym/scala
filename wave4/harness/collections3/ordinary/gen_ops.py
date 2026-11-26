#!/usr/bin/env python3
# Writes SeqLikeOps.scala: one list of ordinary sequence operations, instantiated for Array (ArrayOps),
# String (StringOps) and collection.Seq (immutable and mutable ArraySeq), each compared with the same call on a List.
OPS = r'''
c("slice")(R.slice(i, j))(l.slice(i, j)); c("take")(R.take(i))(l.take(i)); c("drop")(R.drop(i))(l.drop(i))
c("takeRight")(R.takeRight(i))(l.takeRight(i)); c("dropRight")(R.dropRight(i))(l.dropRight(i)); c("splitAt")(R.splitAt(i))(l.splitAt(i))
c("head")(R.head)(l.head); c("last")(R.last)(l.last); c("headOption")(R.headOption)(l.headOption); c("lastOption")(R.lastOption)(l.lastOption)
c("tail")(R.tail)(l.tail); c("init")(R.init)(l.init); c("reverse")(R.reverse)(l.reverse); c("reverseIterator")(R.reverseIterator)(l.reverseIterator); c("iterator")(R.iterator)(l.iterator)
c("takeWhile")(R.takeWhile(p))(l.takeWhile(p)); c("dropWhile")(R.dropWhile(p))(l.dropWhile(p)); c("span")(R.span(p))(l.span(p))
c("filter")(R.filter(p))(l.filter(p)); c("filterNot")(R.filterNot(p))(l.filterNot(p)); c("partition")(R.partition(p))(l.partition(p))
c("count")(R.count(p))(l.count(p)); c("exists")(R.exists(p))(l.exists(p)); c("forall")(R.forall(p))(l.forall(p)); c("find")(R.find(p))(l.find(p))
c("indexWhere")(R.indexWhere(p, i))(l.indexWhere(p, i)); c("lastIndexWhere")(R.lastIndexWhere(p, i))(l.lastIndexWhere(p, i))
c("indexOf")(R.indexOf(x, i))(l.indexOf(x, i)); c("lastIndexOf")(R.lastIndexOf(x, i))(l.lastIndexOf(x, i)); c("contains")(R.contains(x))(l.contains(x))
c("map")(R.map(f))(l.map(f)); c("map.toInt")(R.map(_.hashCode))(l.map(_.hashCode)); c("flatMap")(R.flatMap(e => List(e, x)))(l.flatMap(e => List(e, x)))
c("collect")(R.collect { case e if p(e) => f(e) })(l.collect { case e if p(e) => f(e) }); c("zipWithIndex")(R.zipWithIndex)(l.zipWithIndex)
c("zip")(R.zip(O))(l.zip(lo)); c("zipAll")(R.zipAll(O, x, x))(l.zipAll(lo, x, x))
c("appended")(R.appended(x))(l.appended(x)); c("prepended")(R.prepended(x))(l.prepended(x)); c("appendedAll")(R.appendedAll(O))(l.appendedAll(lo)); c("prependedAll")(R.prependedAll(O))(l.prependedAll(lo))
c("++")(R ++ O)(l ++ lo); c("padTo")(R.padTo(k + i, x))(l.padTo(k + i, x)); c("patch")(R.patch(i, O, j))(l.patch(i, lo, j)); c("updated")(R.updated(i, x))(l.updated(i, x))
c("sorted")(R.sorted(ord))(l.sorted(ord)); c("sortBy")(R.sortBy(e => e)(ord.reverse))(l.sortBy(e => e)(ord.reverse)); c("sortWith")(R.sortWith(ord.lt))(l.sortWith(ord.lt))
c("distinct")(R.distinct)(l.distinct); c("distinctBy")(R.distinctBy(p))(l.distinctBy(p)); c("diff")(R.diff(O))(l.diff(lo)); c("intersect")(R.intersect(O))(l.intersect(lo))
c("grouped")(R.grouped(k))(l.grouped(k)); c("sliding")(R.sliding(k, 1 + k % 3))(l.sliding(k, 1 + k % 3))
if (n <= 30) { c("tails")(R.tails)(l.tails); c("inits")(R.inits)(l.inits) }
if (n <= 10) c("combinations")(R.combinations(2).size)(l.combinations(2).size); if (n <= 5) c("permutations")(R.permutations.size)(l.permutations.size)
c("foldLeft")(R.foldLeft(0)((z, e) => z * 31 + e.hashCode))(l.foldLeft(0)((z, e) => z * 31 + e.hashCode)); c("foldRight")(R.foldRight(0)((e, z) => z * 31 + e.hashCode))(l.foldRight(0)((e, z) => z * 31 + e.hashCode))
c("scanLeft")(R.scanLeft(0)((z, e) => z * 31 + e.hashCode))(l.scanLeft(0)((z, e) => z * 31 + e.hashCode)); c("scanRight")(R.scanRight(0)((e, z) => z * 31 + e.hashCode))(l.scanRight(0)((e, z) => z * 31 + e.hashCode))
c("startsWith")(R.startsWith(O, i.max(0).min(n)))(l.startsWith(lo, i.max(0).min(n))); c("endsWith")(R.endsWith(O))(l.endsWith(lo)); c("startsWith.self")(R.startsWith(R.take(i), 0))(true)
c("min")(R.min(ord))(l.min(ord)); c("max")(R.max(ord))(l.max(ord)); c("mkString")(R.mkString("[", ",", "]"))(l.mkString("[", ",", "]"))
c("lengthCompare")(R.lengthCompare(i).sign)(l.lengthCompare(i).sign); c("size")(R.size)(l.size); c("isEmpty")(R.isEmpty)(l.isEmpty); c("indices")(R.indices)(l.indices)
c("groupBy")(R.groupBy(p))(l.groupBy(p)); c("groupMap")(R.groupMap(p)(f))(l.groupMap(p)(f)); c("partitionMap")(R.partitionMap(e => if (p(e)) Left(e) else Right(f(e))))(l.partitionMap(e => if (p(e)) Left(e) else Right(f(e))))
c("toList")(R.toList)(l); c("toVector")(R.toVector)(l); c("toSet")(R.toSet)(l.toSet); c("toIndexedSeq")(R.toIndexedSeq)(l); c("toArray")(R.toArray[T])(l)
c("copyToArray")({ val b = new Array[T](k + 3); (R.copyToArray(b, i.max(0), j), b) })({ val b = new Array[T](k + 3); (l.copyToArray(b, i.max(0), j), b) })
c("withFilter")(R.withFilter(p).map(f))(l.withFilter(p).map(f)); c("lazyZip")(R.lazyZip(O).map((u, v) => (v, u)))(l.zip(lo).map(_.swap))
c("lazyZip3.filter")(R.lazyZip(lo).lazyZip(O).filter((u, _, _) => p(u)))(l.zip(lo).zip(lo).collect { case ((u, v), w) if p(u) => (u, v, w) }); c("lazyZip4.map")(R.lazyZip(lo).lazyZip(O).lazyZip(l).map((u, v, w, y) => (y, w, v, u)))(l.zip(lo).zip(lo).zip(l).map { case (((u, v), w), y) => (y, w, v, u) })
c("lazyZip.flatMap/exists/forall")((R.lazyZip(O).flatMap((u, v) => List(u, v)), R.lazyZip(O).exists((u, v) => u == v), R.lazyZip(O).forall((u, v) => u == v)))((l.zip(lo).flatMap { case (u, v) => List(u, v) }, l.zip(lo).exists { case (u, v) => u == v }, l.zip(lo).forall { case (u, v) => u == v }))
c("foreach")({ val b = List.newBuilder[T]; R.foreach(b += _); b.result() })(l); c("fold")(R.fold(x)((u, v) => if (ord.lt(u, v)) v else u))(l.fold(x)((u, v) => if (ord.lt(u, v)) v else u)); c("reduceLeftOption")(R.reduceLeftOption((u, v) => if (p(v)) u else v))(l.reduceLeftOption((u, v) => if (p(v)) u else v))
c("appendedAll(List)")(R.appendedAll(lo))(l.appendedAll(lo)); c("prependedAll(List)")(R.prependedAll(lo))(l.prependedAll(lo)); c("++(iterator)")(R ++ lo.iterator)(l ++ lo); c("zip(iterator)")(R.zip(lo.iterator))(l.zip(lo)); c("prependedAll(Vector)")(R.prependedAll(lo.toVector))(lo ++ l)
c("indexOfSlice")(R.indexOfSlice(lo))(l.indexOfSlice(lo)); c("containsSlice")(R.containsSlice(l.slice(i, j)))(l.containsSlice(l.slice(i, j))); c("corresponds")(R.corresponds(lo)(_ == _))(l.corresponds(lo)(_ == _)); c("sameElements")(R.iterator.sameElements(l))(true)
c("search")(R.sorted(ord).search(x)(ord) match { case scala.collection.Searching.Found(at) => R.sorted(ord).apply(at) == x; case ip => ip })(l.sorted(ord).search(x)(ord) match { case scala.collection.Searching.Found(_) => true; case ip => ip }); c("sizeIs")((R.sizeIs > i, R.lengthIs == n, R.sizeCompare(lo.length)))((n > i, true, Integer.compare(n, lo.length)))
c("lazyZip.filter/foreach")((R.lazyZip(O).filter((u, v) => p(u) && !p(v)), { var z = 0; R.lazyZip(O).foreach((u, v) => z = z * 31 + u.hashCode - v.hashCode); z }))((l.zip(lo).filter { case (u, v) => p(u) && !p(v) }, l.zip(lo).foldLeft(0) { case (z, (u, v)) => z * 31 + u.hashCode - v.hashCode }))
c("lazyZip.toIterable")({ val z: Iterable[(T, T)] = R.lazyZip(lo); (z.toList, z.isEmpty, z.knownSize <= z.size, z.iterator.drop(1).toList) })((l.zip(lo), l.isEmpty || lo.isEmpty, true, l.zip(lo).drop(1)))
c("lazyZip3.map/flatMap/exists/forall/foreach")({ val z = R.lazyZip(lo).lazyZip(O); var t = 0; z.foreach((u, v, w) => t += u.hashCode + v.hashCode * 3 + w.hashCode * 5); val it: Iterable[(T, T, T)] = z; (z.map((u, v, w) => (w, v, u)), z.flatMap((u, v, w) => List(u, w)), z.exists((u, v, w) => p(u) && v == w), z.forall((u, v, w) => p(u) || v == w), t, it.toList.size, it.isEmpty) })({ val z = l.zip(lo); (z.map { case (u, v) => (v, v, u) }, z.flatMap { case (u, v) => List(u, v) }, z.exists { case (u, v) => p(u) }, true, z.map { case (u, v) => u.hashCode + v.hashCode * 8 }.sum, z.size, z.isEmpty) })
c("lazyZip4.flatMap/filter/exists/forall/foreach")({ val z = R.lazyZip(lo).lazyZip(O).lazyZip(l); var t = 0; z.foreach((u, v, w, y) => t += u.hashCode + v.hashCode * 3 + w.hashCode * 5 + y.hashCode * 7); val it: Iterable[(T, T, T, T)] = z; (z.flatMap((u, v, w, y) => List(u, w)), z.filter((u, v, w, y) => p(u)).map(_._4), z.exists((u, v, w, y) => p(u) && u == y), z.forall((u, v, w, y) => u == y && v == w), t, it.toList.size, it.isEmpty) })({ val z = l.zip(lo); (z.flatMap { case (u, v) => List(u, v) }, z.collect { case (u, v) if p(u) => u }, z.exists { case (u, v) => p(u) }, true, z.map { case (u, v) => u.hashCode * 8 + v.hashCode * 8 }.sum, z.size, z.isEmpty) })
c("unzip/unzip3")((R.map(e => (e, f(e))).unzip, R.map(e => (e, f(e), n)).unzip3._2))((l.map(e => (e, f(e))).unzip, l.map(f))); c("flatten")(R.map(e => List(e, e)).flatten)(l.flatMap(e => List(e, e)))
c("widen")((R.appended[Any]("w"), R.prepended[Any]("w"), R.padTo[Any](n + 2, "w"), R ++ List[Any]("w")))((l.appended[Any]("w"), l.prepended[Any]("w"), l.padTo[Any](n + 2, "w"), l ++ List[Any]("w")))
c("view.chain")(R.view.drop(i).map(f).filter(p).take(k).concat(List(x)).toList)(l.drop(i).map(f).filter(p).take(k).appended(x)); c("iterator.grouped")(R.iterator.grouped(k).map(_.toList).toList)(l.grouped(k).toList)
c("takeRight/dropWhile chain")(R.takeRight(i).dropWhile(p).reverse.distinct)(l.takeRight(i).dropWhile(p).reverse.distinct); c("to")((R.to(Vector), R.to(scala.collection.mutable.ArrayBuffer), R.toSeq, R.toBuffer))((l, l, l, l))
c("view.slice")(R.view.slice(i, j).toList)(l.slice(i, j)); c("view.reverse")(R.view.reverse.toList)(l.reverse); c("view.takeRight")(R.view.takeRight(i).toList)(l.takeRight(i)); c("view.map")(R.view.map(f).toList)(l.map(f))
c("iterator.drop")(R.iterator.drop(i).take(k))(l.iterator.drop(i).take(k)); c("iterator.sliding")(R.iterator.sliding(k).toList)(l.iterator.sliding(k).toList)
'''
insts = [
  ("arrayOps[T](a: Array[T], o: Array[T]", "a", "o", ""),
  ("seqOps[T](q: collection.Seq[T], qo: collection.Seq[T]", "q", "qo", ""),
  ("strOps(s: String, so: String", "s", "so", "type T = Char; "),
]
out = ["// generated by gen_ops.py; do not edit", "import scala.reflect.ClassTag", "object SeqLikeOps {", "  import OrdDiff.{chk => check}"]
import re
for sig, r, o, pre in insts:
    T = "Char" if pre else "T"
    out.append(f"  def {sig}, l: List[{T}], lo: List[{T}], x: {T}, f: {T} => {T}, i: Int, j: Int, k: Int, fam: String)(implicit ct: ClassTag[{T}], ord: Ordering[{T}]): Unit = {{")
    out.append(f"    {pre}val n = l.length; val p = (e: {T}) => (e.hashCode & 1) == 0")
    out.append(f"    def c(op: String)(act: => Any)(exp: => Any): Unit = check(fam, op, s\"n=$n i=$i j=$j k=$k x=$x\")(exp)(act)")
    body = re.sub(r'\bR\b', r, OPS); body = re.sub(r'\bO\b', o, body)
    out += ["    " + ln for ln in body.strip().split("\n")]
    out.append("  }")
out.append("}")
open("SeqLikeOps.scala", "w").write("\n".join(out) + "\n")
