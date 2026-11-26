// N-version differential tests with programs of calls: mutable buffers, Int sets (incl. sorted and BitSet), maps with colliding keys.
import scala.collection.{immutable => im, mutable => mu}
import scala.util.Random
import java.util.concurrent._
import scala.jdk.CollectionConverters._

final class K(val i: Int) extends Comparable[K] {
  override def hashCode = i % 5
  override def equals(o: Any) = o match { case k: K => k.i == i; case _ => false }
  override def toString = s"K$i"
  def compareTo(o: K) = Integer.compare(i, o.i)
}
object K { implicit val ord: Ordering[K] = Ordering.by(_.i) }

// reference models for the mutating API: an immutable sorted collection in a var; everything else is inherited
final class ModelSet(var s: im.TreeSet[Int]) extends mu.AbstractSet[Int] {
  def addOne(x: Int) = { s += x; this }; def subtractOne(x: Int) = { s -= x; this }
  def contains(x: Int) = s.contains(x); def iterator = s.iterator; def clear() = s = im.TreeSet.empty
}
final class ModelMap(var m: im.TreeMap[K, Int]) extends mu.AbstractMap[K, Int] {
  def addOne(kv: (K, Int)) = { m += kv; this }; def subtractOne(k: K) = { m -= k; this }
  def get(k: K) = m.get(k); def iterator = m.iterator; override def clear() = m = im.TreeMap.empty
}
final class ModelBuf(var v: Vector[Int]) extends mu.AbstractBuffer[Int] {
  def apply(i: Int) = v(i); def length = v.length; def iterator = v.iterator
  def update(i: Int, x: Int) = v = v.updated(i, x)
  def addOne(x: Int) = { v :+= x; this }; def prepend(x: Int) = { v +:= x; this }; def clear() = v = Vector.empty
  private def chk(i: Int) = if (i < 0 || i > v.length) throw new IndexOutOfBoundsException(i.toString)
  def insert(i: Int, x: Int) = { chk(i); v = v.patch(i, List(x), 0) }
  def insertAll(i: Int, xs: IterableOnce[Int]) = { chk(i); val l = xs.iterator.toList; v = v.patch(i, l, 0) }
  def remove(i: Int) = { val x = v(i); v = v.patch(i, Nil, 1); x }
  def remove(i: Int, n: Int) = { if (n < 0) throw new IllegalArgumentException; if (n > 0) { if (i < 0 || i > v.length - n) throw new IndexOutOfBoundsException }; v = v.patch(i, Nil, n) }
  def patchInPlace(from: Int, patch: IterableOnce[Int], replaced: Int) = { val l = patch.iterator.toList; v = v.patch(from, l, replaced); this }
}

object CollDiff {
  final case class Args(i: Int, j: Int, x: Int, n: Int, src: Int, other: List[Int])
  final case class Step(op: Int, a: Args)
  // a family: implementations built from a list, and ops that may mutate and return something
  abstract class Family { self =>
    type C
    def name: String
    def impls: Vector[(String, List[Int] => C)]
    def ops: Vector[(String, (C, Args, List[Int] => C) => Any)]
    def state(c: C): Any
    def elem(r: Random): Int
    def run(mk: List[Int] => C, xs: List[Int], prog: List[Step]): List[Any] = {
      val out = List.newBuilder[Any]
      try {
        val c = mk(xs)
        var p = prog; var ok = true
        while (ok && p.nonEmpty) {
          val st = p.head; p = p.tail
          val res = try norm(ops(st.op)._2(c, st.a, mk)) catch { case _: IndexOutOfBoundsException => ok = false; "EX:IndexOutOfBoundsException"; case _: StackOverflowError => ok = false; "EX:StackOverflowError"; case e: Exception => ok = false; "EX:" + e.getClass.getSimpleName }
          out += res
          if (ok) out += norm(state(c))
        }
      } catch { case e: Throwable => out += "EX-outer:" + e.getClass.getSimpleName }
      out.result()
    }
  }
  def norm(a: Any): Any = a match {
    case it: Iterator[_] => it.take(100000).map(norm).toList
    case arr: Array[_] => arr.toList.map(norm)
    case m: scala.collection.Map[_, _] => m.iterator.map(norm).toSet
    case s: scala.collection.Set[_] => s.iterator.map(norm).toSet
    case it: Iterable[_] => it.iterator.take(100000).map(norm).toList
    case p: Product => p.productPrefix :: p.productIterator.map(norm).toList
    case x => x
  }

  object Buf extends Family {
    type C = mu.Buffer[Int]
    def name = "buf"
    def elem(r: Random) = r.nextInt(9) - 2
    def state(c: C) = (c.toList, c.length, c.iterator.toList, c.reverseIterator.toList, c.knownSize match { case -1 => c.length; case n => n })
    lazy val impls: Vector[(String, List[Int] => C)] = Vector(
      "Model" -> (xs => new ModelBuf(xs.toVector)),
      "ArrayBuffer" -> (xs => mu.ArrayBuffer.from(xs)),
      "ListBuffer" -> (xs => mu.ListBuffer.from(xs)),
      "ListBuffer.aliased" -> (xs => { val b = mu.ListBuffer.from(xs); b.toList; b }),
      "ArrayDeque" -> (xs => mu.ArrayDeque.from(xs)),
      "ArrayDeque.wrapped" -> (xs => { val d = new mu.ArrayDeque[Int](16); (1 to 10).foreach(d += _); (1 to 10).foreach(_ => d.removeHead()); val (a, b) = xs.splitAt(xs.length / 2); d ++= b; a.reverseIterator.foreach(d.prepend); d }),
      "mu.Queue" -> (xs => mu.Queue.from(xs)),
      "mu.Stack" -> (xs => mu.Stack.from(xs)),
      "UnrolledBuffer" -> (xs => mu.UnrolledBuffer.from(xs)),
      "juArrayList" -> (xs => { val l = new java.util.ArrayList[Int]; xs.foreach(l.add); l.asScala }),
      "juLinkedList" -> (xs => { val l = new java.util.LinkedList[Int]; xs.foreach(l.add); l.asScala }),
      "ArrayBuffer.sized" -> (xs => { val b = new mu.ArrayBuffer[Int](xs.length); b ++= xs; b }),
    )
    def src(c: C, a: Args, mk: List[Int] => C): IterableOnce[Int] = a.src match {
      case 0 => a.other; case 1 => c; case 2 => c.view; case 3 => c.iterator; case 4 => c.view.reverse; case 5 => c.view.map(_ + 1)
      case 6 => c.toList; case 7 => c.view.filter(_ > a.x); case 8 => mk(a.other); case 9 => c.view.slice(a.j, a.n); case 10 => c.reverseIterator; case _ => a.other.iterator
    }
    val ops: Vector[(String, (C, Args, List[Int] => C) => Any)] = Vector(
      "append" -> ((c, a, _) => { c.append(a.x); () }), "prepend" -> ((c, a, _) => { c.prepend(a.x); () }),
      "insert" -> ((c, a, _) => c.insert(a.i, a.x)), "insertAll" -> ((c, a, mk) => c.insertAll(a.i, src(c, a, mk))),
      "prependAll" -> ((c, a, mk) => { c.prependAll(src(c, a, mk)); () }), "addAll" -> ((c, a, mk) => { c.addAll(src(c, a, mk)); () }),
      "remove" -> ((c, a, _) => c.remove(a.i)), "remove2" -> ((c, a, _) => c.remove(a.i, a.n)),
      "update" -> ((c, a, _) => c.update(a.i, a.x)), "patchInPlace" -> ((c, a, mk) => { c.patchInPlace(a.i, src(c, a, mk), a.j); () }),
      "dropInPlace" -> ((c, a, _) => { c.dropInPlace(a.i); () }), "dropRightInPlace" -> ((c, a, _) => { c.dropRightInPlace(a.i); () }),
      "takeInPlace" -> ((c, a, _) => { c.takeInPlace(a.i); () }), "takeRightInPlace" -> ((c, a, _) => { c.takeRightInPlace(a.i); () }),
      "sliceInPlace" -> ((c, a, _) => { c.sliceInPlace(a.i, a.j); () }), "filterInPlace" -> ((c, a, _) => { c match { case b: mu.IndexedBuffer[Int @unchecked] => b.filterInPlace(_ > a.x); case b: mu.ListBuffer[Int @unchecked] => b.filterInPlace(_ > a.x); case _ => c.patchInPlace(0, c.toList.filter(_ > a.x), c.length) }; () }),
      "mapInPlace" -> ((c, a, _) => { c match { case b: mu.IndexedBuffer[Int @unchecked] => b.mapInPlace(_ + 1); case b: mu.ListBuffer[Int @unchecked] => b.mapInPlace(_ + 1); case _ => c.patchInPlace(0, c.toList.map(_ + 1), c.length) }; () }), "flatMapInPlace" -> ((c, a, _) => { val f = (e: Int) => if (e > a.x) List(e, e) else Nil; c match { case b: mu.IndexedBuffer[Int @unchecked] => b.flatMapInPlace(f); case b: mu.ListBuffer[Int @unchecked] => b.flatMapInPlace(f); case _ => c.patchInPlace(0, c.toList.flatMap(f), c.length) }; () }),
      "padToInPlace" -> ((c, a, _) => { c.padToInPlace(a.n, a.x); () }), "clear" -> ((c, _, _) => c.clear()),
      "subtractOne" -> ((c, a, _) => { c.subtractOne(a.x); () }), "subtractAll" -> ((c, a, mk) => { c.subtractAll(src(c, a, mk)); () }),
      "dropWhileInPlace" -> ((c, a, _) => { c.dropWhileInPlace(_ > a.x); () }), "takeWhileInPlace" -> ((c, a, _) => { c.takeWhileInPlace(_ > a.x); () }),
      "trimStart" -> ((c, a, _) => (c.trimStart(a.n): @annotation.nowarn)), "trimEnd" -> ((c, a, _) => (c.trimEnd(a.n): @annotation.nowarn)),
      "++=:" -> ((c, a, mk) => { src(c, a, mk) ++=: c; () }), "clone.eq" -> ((c, a, _) => { val d = c.clone(); d.append(99); (c == d, d.toList) }),
      "slice" -> ((c, a, _) => c.slice(a.i, a.j)), "indexOf" -> ((c, a, _) => c.indexOf(a.x, a.i)), "apply" -> ((c, a, _) => c(a.i)),
      "iter.mid" -> ((c, a, _) => { val it = c.iterator.drop(a.i); val k = it.knownSize; val l = it.toList; (k == -1 || k == l.size, l) }),
      "distinctBy.sorted" -> ((c, a, _) => (c.sorted, c.reverse, c.distinct)), "sortInPlace" -> ((c, _, _) => c match { case i: mu.IndexedSeq[Int @unchecked] => i.sortInPlace(); () ; case _ => () }),
      "hash" -> ((c, _, _) => (c.hashCode, c == c.toList, c.toList == c)),
    )
  }

  class SetFam(mutableOnly: Boolean) extends Family {
    type C = scala.collection.Set[Int]
    def name = if (mutableOnly) "mset" else "set"
    def elem(r: Random) = r.nextInt(8) match { case 0 => 62 + r.nextInt(4); case 1 => 126 + r.nextInt(4); case 2 => r.nextInt(600); case 3 => 0; case _ => r.nextInt(20) }
    def state(c: C) = (c.size, c.toList.sorted, c.hashCode)
    def jset(s: java.util.Set[Int], xs: List[Int]) = { xs.foreach(s.add); s.asScala }
    def impls = if (mutableOnly) (("Model", (xs: List[Int]) => new ModelSet(im.TreeSet.from(xs)): C)) +: impls0.filter(_._2(Nil).isInstanceOf[mu.Set[_]]) else impls0
    def ops = if (mutableOnly) ops0 else ops0.filterNot(_._1.startsWith("m."))
    val impls0: Vector[(String, List[Int] => C)] = Vector(
      "im.TreeSet" -> (xs => im.TreeSet.from(xs)), "im.HashSet" -> (xs => im.HashSet.from(xs)), "im.Set" -> (xs => im.Set.from(xs)),
      "im.Set.incr" -> (xs => xs.foldLeft(im.Set.empty[Int])(_ + _)), "im.HashSet.rem" -> (xs => (im.HashSet.from(xs) ++ (1000 to 1100)) -- (1000 to 1100)),
      "im.BitSet" -> (xs => im.BitSet.fromSpecific(xs)), "im.BitSet.incr" -> (xs => xs.foldLeft(im.BitSet.empty)(_ + _)),
      "im.BitSet.rem" -> (xs => (im.BitSet.fromSpecific(xs) + 5000) - 5000 match { case b if xs.contains(5000) => b + 5000; case b => b }),
      "ListSet" -> (xs => im.ListSet.from(xs)), "im.SortedSet" -> (xs => im.SortedSet.from(xs)),
      "mu.HashSet" -> (xs => mu.HashSet.from(xs)), "mu.LinkedHashSet" -> (xs => mu.LinkedHashSet.from(xs)), "mu.TreeSet" -> (xs => mu.TreeSet.from(xs)),
      "mu.BitSet" -> (xs => mu.BitSet.fromSpecific(xs)), "juTreeSet" -> (xs => jset(new java.util.TreeSet[Int], xs)), "juHashSet" -> (xs => jset(new java.util.HashSet[Int], xs)),
      "keySet" -> (xs => im.HashMap.from(xs.map(x => x -> x)).keySet), "TreeMap.keySet" -> (xs => im.TreeMap.from(xs.map(x => x -> x)).keySet),
      "mu.TreeSet.range" -> (xs => (mu.TreeSet.from(xs) ++= List(-5, 9000)).range(0, 8999)), "im.TreeSet.range" -> (xs => (im.TreeSet.from(xs) ++ List(-5, 9000)).range(0, 8999)),
      "muHashMap.keySet" -> (xs => mu.HashMap.from(xs.map(x => x -> x)).keySet), "muTreeMap.keySet" -> (xs => mu.TreeMap.from(xs.map(x => x -> x)).keySet),
    )
    def sorted(c: C)(f: scala.collection.SortedSet[Int] => Any): Any = c match { case s: scala.collection.SortedSet[Int @unchecked] => f(s); case _ => f(im.TreeSet.from(c)) }
    def imm(c: C)(f: im.Set[Int] => Any): Any = c match { case s: im.Set[Int @unchecked] => f(s); case _ => f(im.TreeSet.from(c)) }
    def mut(c: C)(f: mu.Set[Int] => Any): Any = c match { case s: mu.Set[Int @unchecked] => f(s); case _ => () }
    val ops0: Vector[(String, (C, Args, List[Int] => C) => Any)] = Vector(
      "contains" -> ((c, a, _) => (c.contains(a.x), c(a.i), c.contains(-1))), "union" -> ((c, a, mk) => c.union(mk(a.other))), "concat" -> ((c, a, mk) => c ++ a.other),
      "diff" -> ((c, a, mk) => c.diff(mk(a.other))), "intersect" -> ((c, a, mk) => c.intersect(mk(a.other))), "subsetOf" -> ((c, a, mk) => (c.subsetOf(mk(a.other)), mk(a.other).subsetOf(c), mk(a.other.take(2)).subsetOf(c))),
      "equals" -> ((c, a, mk) => (c == mk(a.other), mk(a.other) == c, c == mk(c.toList), mk(c.toList.reverse).hashCode == c.hashCode)),
      "filter" -> ((c, a, _) => c.filter(_ > a.x)), "partition" -> ((c, a, _) => c.partition(_ % 3 == 0)), "map" -> ((c, a, _) => c.map(_ / 2)), "map.up" -> ((c, a, _) => c.map(_ * 67 + 1)),
      "flatMap" -> ((c, a, _) => c.flatMap(e => List(e, e + 64))), "collect" -> ((c, a, _) => c.collect { case e if e > a.x => e + 1 }), "count" -> ((c, a, _) => (c.count(_ > a.x), c.sum, c.maxOption, c.minOption)),
      "foldLeft" -> ((c, _, _) => c.foldLeft(0L)(_ + _)), "groupBy" -> ((c, _, _) => c.groupBy(_ % 3)), "subsets" -> ((c, a, _) => c.take(0).subsets().size),
      "iter.size" -> ((c, a, _) => { val it = c.iterator.drop(a.i max 0 min 3); val k = it.knownSize; val n = it.size; (k == -1 || k == n, n) }),
      "sizeCompare" -> ((c, a, _) => (Integer.signum(c.sizeCompare(a.n)), c.isEmpty, c.knownSize == -1 || c.knownSize == c.size)),
      "s.range" -> ((c, a, _) => sorted(c)(s => s.range(a.i, a.j).toList)), "s.rangeFrom" -> ((c, a, _) => sorted(c)(s => (s.rangeFrom(a.i).toList, s.rangeTo(a.j).toList, s.rangeUntil(a.j).toList))),
      "s.minAfter" -> ((c, a, _) => sorted(c)(s => (s.minAfter(a.i), s.maxBefore(a.i), s.headOption, s.lastOption))), "s.iteratorFrom" -> ((c, a, _) => sorted(c)(s => s.iteratorFrom(a.i).toList)),
      "s.range.ops" -> ((c, a, _) => sorted(c)(s => { val r = s.range(a.i, a.j); (r.size, r.contains(a.x), r.headOption, r.lastOption, r.minAfter(a.x), r.maxBefore(a.x), r.iteratorFrom(a.x).toList, r.rangeFrom(a.x).toList, r.toList.reverse) })),
      "s.slice" -> ((c, a, _) => sorted(c)(s => (s.take(a.n).toList, s.drop(a.n).toList, s.slice(a.n, a.n + 2).toList, s.takeRight(a.n).toList, s.dropRight(a.n).toList, s.splitAt(a.n)._2.toList))),
      "s.span" -> ((c, a, _) => sorted(c)(s => (s.takeWhile(_ < a.i).toList, s.dropWhile(_ < a.i).toList, s.span(_ < a.i)._1.toList, s.init.toList, s.tail.toList))),
      "s.stepper" -> ((c, a, _) => sorted(c)(s => { val st = s.stepper; val sp = st.trySplit(); ((if (sp == null) Nil else sp.iterator.toList) ::: st.iterator.toList) })),
      "s.rev" -> ((c, a, _) => sorted(c)(s => (s.toList.reverse, s.lastOption, s.foldRight(List.empty[Int])(_ :: _), s.zipWithIndex.toList.sorted, s.unsorted.size))),
      "i.incl" -> ((c, a, _) => imm(c)(s => (s + a.x, s - a.x, s + (a.i & 1023) - (a.i & 1023), s.removedAll(a.other), s concat a.other, s.excl(a.x).incl(a.x).size))),
      "i.xor" -> ((c, a, mk) => imm(c)(s => (s &~ mk(a.other), s | mk(a.other), s & mk(a.other), s.filterNot(mk(a.other))))),
      "m.add" -> ((c, a, _) => mut(c)(s => (s.add(a.x), s.remove(a.i & 1023), s.add(a.x)))), "m.addAll" -> ((c, a, mk) => mut(c)(s => { s ++= (a.src match { case 0 => a.other; case 1 => s.toList; case 2 => mk(a.other); case _ => a.other.iterator }); () })),
      "m.subtractAll" -> ((c, a, mk) => mut(c)(s => { s --= (a.src match { case 0 => a.other; case 1 => s.toList.take(2); case 2 => mk(a.other); case _ => a.other.iterator }); () })),
      "m.filterInPlace" -> ((c, a, _) => mut(c)(s => { s.filterInPlace(_ % 3 != a.n.abs % 3); () })), "m.clone" -> ((c, a, _) => mut(c)(s => { val d = s.clone(); d += 777; (s.contains(777), d.size - s.size) })),
      "m.update" -> ((c, a, _) => mut(c)(s => { s(a.x) = a.n > 2; s(a.i & 1023) = false; () })), "m.intersect" -> ((c, a, mk) => mut(c)(s => { s.filterInPlace(mk(a.other)); () })), "m.clear" -> ((c, a, _) => mut(c)(s => if (a.n > 4) s.clear())),
    )
  }

  class MapFam(mutableOnly: Boolean) extends Family {
    type C = scala.collection.Map[K, Int]
    def name = if (mutableOnly) "mmap" else "map"
    def elem(r: Random) = r.nextInt(25)
    def state(c: C) = (c.size, c.toList.sortBy(_._1), c.hashCode, c.keySet.size, c.values.sum)
    def kv(xs: List[Int]) = xs.zipWithIndex.map { case (x, i) => new K(x) -> i }
    def jmap(m: java.util.Map[K, Int], xs: List[Int]) = { kv(xs).foreach { case (k, v) => m.put(k, v) }; m.asScala }
    def impls = if (mutableOnly) (("Model", (xs: List[Int]) => new ModelMap(im.TreeMap.from(kv(xs))): C)) +: impls0.filter(_._2(Nil).isInstanceOf[mu.Map[_, _]]) else impls0
    def ops = if (mutableOnly) ops0 else ops0.filterNot(_._1.startsWith("m."))
    val impls0: Vector[(String, List[Int] => C)] = Vector(
      "im.TreeMap" -> (xs => im.TreeMap.from(kv(xs))), "im.HashMap" -> (xs => im.HashMap.from(kv(xs))), "im.Map" -> (xs => im.Map.from(kv(xs))),
      "im.Map.incr" -> (xs => kv(xs).foldLeft(im.Map.empty[K, Int])(_ + _)), "im.HashMap.rem" -> (xs => (im.HashMap.from(kv(xs)) ++ (1000 to 1100).map(i => new K(i) -> i)) -- (1000 to 1100).map(new K(_))),
      "ListMap" -> (xs => im.ListMap.from(kv(xs))), "VectorMap" -> (xs => im.VectorMap.from(kv(xs))), "TreeSeqMap" -> (xs => im.TreeSeqMap.from(kv(xs))), "SeqMap" -> (xs => im.SeqMap.from(kv(xs))),
      "VectorMap.rem" -> (xs => (im.VectorMap.from(kv(xs)) ++ (1000 to 1010).map(i => new K(i) -> i)) -- (1000 to 1010).map(new K(_))), "TreeSeqMap.mod" -> (xs => im.TreeSeqMap.empty[K, Int](im.TreeSeqMap.OrderBy.Modification) ++ kv(xs)),
      "mu.HashMap" -> (xs => mu.HashMap.from(kv(xs))), "mu.LinkedHashMap" -> (xs => mu.LinkedHashMap.from(kv(xs))), "mu.TreeMap" -> (xs => mu.TreeMap.from(kv(xs))),
      "CollisionProofHashMap" -> (xs => mu.CollisionProofHashMap.from(kv(xs))), "AnyRefMap" -> (xs => (mu.AnyRefMap.from(kv(xs)): @annotation.nowarn)), "TrieMap" -> (xs => scala.collection.concurrent.TrieMap.from(kv(xs))),
      "juHashMap" -> (xs => jmap(new java.util.HashMap[K, Int], xs)), "juTreeMap" -> (xs => jmap(new java.util.TreeMap[K, Int], xs)), "juCHM" -> (xs => jmap(new java.util.concurrent.ConcurrentHashMap[K, Int], xs)),
      "mu.HashMap.small" -> (xs => { val m = new mu.HashMap[K, Int](2, 4.0); m ++= kv(xs); m }), "im.TreeMap.range" -> (xs => (im.TreeMap.from(kv(xs)) + (new K(-5) -> 0) + (new K(9000) -> 0)).range(new K(0), new K(8999))),
      "mu.TreeMap.range" -> (xs => (mu.TreeMap.from(kv(xs)) += (new K(-5) -> 0) += (new K(9000) -> 0)).range(new K(0), new K(8999))), "withDefault" -> (xs => im.HashMap.from(kv(xs)).withDefaultValue(-1).filter(_ => true)),
    )
    def sorted(c: C)(f: scala.collection.SortedMap[K, Int] => Any): Any = c match { case s: scala.collection.SortedMap[K @unchecked, Int @unchecked] => f(s); case _ => f(im.TreeMap.from(c)) }
    def imm(c: C)(f: im.Map[K, Int] => Any): Any = c match { case s: im.Map[K @unchecked, Int @unchecked] => f(s); case _ => f(im.TreeMap.from(c)) }
    def mut(c: C)(f: mu.Map[K, Int] => Any): Any = c match { case s: mu.Map[K @unchecked, Int @unchecked] => f(s); case _ => () }
    val ops0: Vector[(String, (C, Args, List[Int] => C) => Any)] = Vector(
      "get" -> ((c, a, _) => (c.get(new K(a.x)), c.contains(new K(a.i)), c.getOrElse(new K(a.x), -9), c.isDefinedAt(new K(a.x)))), "concat" -> ((c, a, mk) => c ++ mk(a.other)), "concat.l" -> ((c, a, _) => c ++ kv(a.other)),
      "equals" -> ((c, a, mk) => (c == mk(a.other), mk(a.other) == c, c == im.TreeMap.from(c), im.HashMap.from(c).hashCode == c.hashCode)),
      "filter" -> ((c, a, _) => c.filter(_._1.i > a.x)), "partition" -> ((c, a, _) => c.partition(_._2 % 2 == 0)), "map" -> ((c, a, _) => c.map { case (k, v) => new K(k.i / 2) -> 1 }.keySet),
      "map.v" -> ((c, a, _) => c.map { case (k, v) => k -> (v + 1) }), "flatMap" -> ((c, a, _) => c.flatMap { case (k, v) => List(k -> v, new K(k.i + 5) -> 0) }.keySet), "collect" -> ((c, a, _) => c.collect { case (k, v) if k.i > a.x => k -> v }),
      "keySet" -> ((c, a, mk) => (c.keySet, c.keySet.contains(new K(a.x)), c.keySet == mk(a.other).keySet, c.keySet.filter(_.i > a.x), c.keySet.map(_.i).sum)), "values" -> ((c, _, _) => (c.values.toList.sorted, c.valuesIterator.sum, c.keysIterator.map(_.i).sum)),
      "views" -> ((c, a, _) => (c.view.filterKeys(_.i > a.x).toMap, c.view.mapValues(_ + 1).toMap, c.view.filterKeys(_.i > a.x).get(new K(a.i)))), "count" -> ((c, a, _) => (c.count(_._1.i > a.x), c.exists(_._1.i == a.x), c.find(_._1.i == a.x), c.foldLeft(0)(_ + _._2))),
      "iter" -> ((c, a, _) => { val it = c.iterator.drop(a.i max 0 min 3); val k = it.knownSize; val n = it.size; (k == -1 || k == n, n, c.knownSize == -1 || c.knownSize == c.size) }), "groupBy" -> ((c, _, _) => c.groupBy(_._1.i % 3)),
      "s.range" -> ((c, a, _) => sorted(c)(s => (s.range(new K(a.i), new K(a.j)).toList, s.rangeFrom(new K(a.i)).toList, s.rangeTo(new K(a.j)).toList, s.rangeUntil(new K(a.j)).toList))),
      "s.minAfter" -> ((c, a, _) => sorted(c)(s => (s.minAfter(new K(a.i)), s.maxBefore(new K(a.i)), s.headOption, s.lastOption, s.firstKey, s.lastKey))),
      "s.iteratorFrom" -> ((c, a, _) => sorted(c)(s => (s.iteratorFrom(new K(a.i)).toList, s.keysIteratorFrom(new K(a.i)).toList, s.valuesIteratorFrom(new K(a.i)).toList))),
      "s.range.ops" -> ((c, a, _) => sorted(c)(s => { val r = s.range(new K(a.i), new K(a.j)); val x = new K(a.x); (r.size, r.contains(x), r.get(x), r.headOption, r.lastOption, r.minAfter(x), r.maxBefore(x), r.iteratorFrom(x).toList, r.keySet.toList, r.toList.reverse, r.foldLeft(0)(_ + _._2)) })),
      "s.slice" -> ((c, a, _) => sorted(c)(s => (s.take(a.n).toList, s.drop(a.n).toList, s.takeRight(a.n).toList, s.init.toList, s.tail.toList, s.keySet.range(new K(a.i), new K(a.j)).toList, s.keySet.minAfter(new K(a.i))))),
      "i.updated" -> ((c, a, _) => imm(c)(s => (s.updated(new K(a.x), 7), s.removed(new K(a.x)), s + (new K(a.i) -> 1) - new K(a.i), s.removedAll(a.other.map(new K(_))), s.updated(new K(a.x), 7).removed(new K(a.x)).size))),
      "i.updatedWith" -> ((c, a, _) => imm(c)(s => (s.updatedWith(new K(a.x))(_.map(_ + 1)), s.updatedWith(new K(a.x))(_ => None), s.updatedWith(new K(a.i))(_ => Some(3)), s.transform((k, v) => v + k.i)))),
      "i.concat" -> ((c, a, mk) => imm(c)(s => (s ++ mk(a.other), s.concat(kv(a.other)), s.keySet ++ mk(a.other).keySet, s.keySet -- mk(a.other).keySet, s.filterNot(kv => mk(a.other).contains(kv._1))))),
      "m.put" -> ((c, a, _) => mut(c)(s => (s.put(new K(a.x), 5), s.remove(new K(a.i)), s.getOrElseUpdate(new K(a.x), 8), s.updateWith(new K(a.i))(_ => if (a.n > 3) Some(1) else None)))),
      "m.addAll" -> ((c, a, mk) => mut(c)(s => { s ++= (a.src match { case 0 => kv(a.other); case 1 => s.toList; case 2 => mk(a.other); case _ => kv(a.other).iterator }); () })),
      "m.subtractAll" -> ((c, a, mk) => mut(c)(s => { s --= (a.src match { case 0 => a.other.map(new K(_)); case 1 => s.keys.toList.take(2); case 2 => mk(a.other).keySet; case _ => a.other.iterator.map(new K(_)) }); () })),
      "m.filterInPlace" -> ((c, a, _) => mut(c)(s => { s.filterInPlace((k, v) => k.i % 3 != a.n.abs % 3); () })), "m.mapValuesInPlace" -> ((c, a, _) => mut(c)(s => { s.mapValuesInPlace((k, v) => v + k.i); () })),
      "m.clone" -> ((c, a, _) => mut(c)(s => { val d = s.clone(); d(new K(777)) = 1; (s.contains(new K(777)), d.size - s.size) })), "m.clear" -> ((c, a, _) => mut(c)(s => if (a.n > 4) s.clear())),
      "m.updateIter" -> ((c, a, _) => mut(c)(s => { var n = 0; for (k <- s.keys.toList) { s(k) = s(k) + 1; n += 1 }; n })), "m.remove.iter" -> ((c, a, _) => mut(c)(s => { val ks = s.keysIterator.toList; ks.take(a.n.abs % 4).foreach(s.remove); s.size })),
    )
  }

  def idx(r: Random, len: Int): Int = r.nextInt(16) match {
    case 0 => -1; case 1 => 0; case 2 => 1; case 3 => len - 1; case 4 => len; case 5 => len + 1
    case 6 => Int.MinValue; case 7 => Int.MaxValue; case 8 => Int.MaxValue - 1; case 9 => Int.MinValue + 1
    case 10 => len / 2; case 11 => 63 + r.nextInt(3); case 12 => Int.MaxValue - len; case _ => r.nextInt(len + 3)
  }
  val sizes = Array(0, 1, 2, 3, 4, 5, 8, 15, 16, 17, 31, 32, 33, 64, 65, 100, 300)

  def main(argv: Array[String]): Unit = {
    val fam: Family = argv(0) match { case "buf" => Buf; case "set" => new SetFam(false); case "mset" => new SetFam(true); case "map" => new MapFam(false); case "mmap" => new MapFam(true) }
    val seed = argv(1).toLong; val budgetMs = argv(2).toLong * 1000
    val r = new Random(seed)
    val pool = Executors.newCachedThreadPool((rn: Runnable) => { val t = new Thread(rn); t.setDaemon(true); t })
    def timed[T](f: => T): Option[T] = { val fu = pool.submit(new Callable[T] { def call = f }); try Some(fu.get(4, TimeUnit.SECONDS)) catch { case _: TimeoutException => fu.cancel(true); None } }
    def genList() = { val n = r.nextInt(8) match { case 0 => sizes(r.nextInt(sizes.length)); case 1 => r.nextInt(40); case _ => r.nextInt(7) }; List.fill(n)(fam.elem(r)) }
    val seen = mu.LinkedHashMap.empty[String, Int]; val quarantine = mu.HashSet.empty[(String, String)]
    val t0 = System.currentTimeMillis; var cases = 0L
    val ref = fam.impls(0)
    while (System.currentTimeMillis - t0 < budgetMs) {
      val xs = genList()
      val prog = List.fill(1 + r.nextInt(4)) {
        val len = xs.length
        val n = r.nextInt(10) match { case 0 => -1; case 1 => 0; case 2 => Int.MinValue; case 3 => len; case 4 => len + 1; case 5 => Int.MaxValue; case _ => 1 + r.nextInt(6) }
        val other = r.nextInt(5) match { case 0 => xs; case 1 => Nil; case 2 => xs.take(r.nextInt(len + 1)); case _ => List.fill(r.nextInt(5))(fam.elem(r)) }
        Step(r.nextInt(fam.ops.length), Args(idx(r, len), idx(r, len), fam.elem(r), n, r.nextInt(12), other))
      }
      // padTo with a huge length is a legitimate OOM; keep n small there
      val prog1 = prog.map(s => if (fam.ops(s.op)._1.startsWith("padTo") && s.a.n > 1000) s.copy(a = s.a.copy(n = 9)) else s)
      cases += 1
      timed(fam.run(ref._2, xs, prog1)) match {
        case None => println(s"HANG ref $xs ${prog1.map(s => fam.ops(s.op)._1 -> s.a)}")
        case Some(want) =>
          for ((name, mk) <- fam.impls.tail if !prog1.exists(s => quarantine((fam.ops(s.op)._1, name)))) {
            def differs(x: List[Int], p: List[Step]): Option[String] = timed { val w = fam.run(ref._2, x, p); val g = fam.run(mk, x, p); if (w == g) None else {
              val at = w.zip(g).indexWhere { case (a, b) => a != b } match { case -1 => w.length min g.length; case i => i }
              val lastOp = fam.ops(p(((at min (2 * p.length - 1)) / 2) min (p.length - 1)).op)._1
              def ex(l: List[Any]) = l.lift(at).exists(_.toString.startsWith("EX"))
              Some(s"$lastOp | $name | " + (if (ex(w) && ex(g)) "exc-type" else if (ex(w)) "value-for-exc" else if (ex(g)) "exc-for-value" else "value")) } }.getOrElse(Some(s"? | $name | hang"))
            differs(xs, prog1) match {
              case None =>
              case Some(sig) =>
                val c = seen.getOrElse(sig, 0); seen(sig) = c + 1
                if (sig.endsWith("hang") || fam.run(mk, xs, prog1).exists(_.toString.contains("OutOfMemory"))) prog1.foreach(s => quarantine += ((fam.ops(s.op)._1, name)))
                if (c == 0 && !sig.endsWith("exc-type")) {
                  var cx = xs; var cp = prog1; var progress = !sig.endsWith("hang")
                  while (progress) {
                    progress = false
                    val cands = cp.indices.map(i => (cx, cp.patch(i, Nil, 1))).filter(_._2.nonEmpty) ++ cx.indices.map(i => (cx.patch(i, Nil, 1), cp)) ++
                      cp.indices.flatMap(i => { val a = cp(i).a; Seq(a.copy(other = Nil), a.copy(other = a.other.drop(1)), a.copy(i = 0), a.copy(j = 0), a.copy(n = 1), a.copy(src = 0), a.copy(x = 0)).filter(_ != a).map(na => (cx, cp.updated(i, cp(i).copy(a = na)))) })
                    cands.find { case (x, p) => differs(x, p).contains(sig) }.foreach { case (x, p) => cx = x; cp = p; progress = true }
                  }
                  def cut(s: Any) = { val t = s.toString; if (t.length > 400) t.take(400) + "..." else t }
                  println(s"NEW $sig\n  xs=${cut(cx)}\n  prog=${cp.map(s => fam.ops(s.op)._1 + s.a)}\n    ${ref._1}: ${cut(fam.run(ref._2, cx, cp))}\n    $name: ${cut(fam.run(mk, cx, cp))}")
                }
            }
          }
      }
    }
    println(s"quarantined after a hang or OutOfMemoryError (op, impl): ${quarantine.toList.sorted.mkString(", ")}")
    println(s"\n==== ${fam.name} seed $seed: $cases programs x ${fam.impls.length - 1} impls, ${fam.ops.length} ops, ${seen.size} signatures")
    seen.toList.sortBy(_._1).foreach { case (sig, c) => println(f"$c%7d  $sig") }
    System.exit(0)
  }
}
