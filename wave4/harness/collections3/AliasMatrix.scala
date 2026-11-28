// Self-aliasing matrix: every bulk method of every mutable collection, fed a source derived from the collection itself.
// Model: the argument is read as it was before the call. The expected state comes from running the same call with the
// source derived from a second, equal instance that the call does not touch.
// usage: AliasMatrix <out.tsv> <first cell index> [mark]   (exit status 3 after a cell that did not terminate: run again to resume; see run.sh)
import scala.collection.{mutable => mu, concurrent => cc}
import scala.jdk.CollectionConverters._
import java.util.ConcurrentModificationException

object AliasMatrix {
  object NA extends RuntimeException("n/a") with scala.util.control.NoStackTrace
  // run(init) = (actual, expected), each returning the final state
  final case class Cell(fam: String, impl: String, method: String, source: String, run: List[Int] => (() => Any, () => Any))

  def cells[C](fam: String, impls: List[(String, List[Int] => C)], state: C => Any)(ops: List[(String, String, (C, C) => Any, Option[C => Any])]): List[Cell] =
    for ((in, mk) <- impls; (m, s, op, ref) <- ops) yield Cell(fam, in, m, s, init => (
      () => { val c = mk(init); op(c, c); state(c) },
      () => ref match { case Some(r) => val t = mk(init); r(t); state(t); case None => val t = mk(init); val d = mk(init); op(t, d); state(t) }))

  type Src[A] = List[(String, Iterable[A] => IterableOnce[A])]
  def elemSources(f: Int => Int): Src[Int] = List(
    "self" -> (d => d), "view" -> (d => d.view), "view.map" -> (d => d.view.map(f)), "view.filter" -> (d => d.view.filter(_ % 2 == 1)),
    "view.take(2)" -> (d => d.view.take(2)), "iterator" -> (d => d.iterator), "iterator.map" -> (d => d.iterator.map(f)),
    "to(LazyList)" -> (d => d.to(LazyList)), "toList (control)" -> (d => d.toList))
  val seqSources: List[(String, mu.Buffer[Int] => IterableOnce[Int])] = elemSources(_ + 1).map { case (n, s) => (n, s: mu.Buffer[Int] => IterableOnce[Int]) } ++ List[(String, mu.Buffer[Int] => IterableOnce[Int])](
    "view.reverse" -> (d => d.view.reverse), "reverseIterator" -> (d => d.reverseIterator), "view.slice(1,3)" -> (d => d.view.slice(1, 3)))

  // ---- sets
  val setImpls: List[(String, List[Int] => mu.Set[Int])] = List(
    "HashSet" -> (xs => mu.HashSet.from(xs)), "LinkedHashSet" -> (xs => mu.LinkedHashSet.from(xs)), "TreeSet" -> (xs => mu.TreeSet.from(xs)), "BitSet" -> (xs => mu.BitSet.fromSpecific(xs)),
    "ju.HashSet.asScala" -> (xs => { val s = new java.util.HashSet[Int]; xs.foreach(s.add); s.asScala }),
    "ju.TreeSet.asScala" -> (xs => { val s = new java.util.TreeSet[Int]; xs.foreach(s.add); s.asScala }),
    "ju.LinkedHashSet.asScala" -> (xs => { val s = new java.util.LinkedHashSet[Int]; xs.foreach(s.add); s.asScala }))
  def setCells = cells[mu.Set[Int]]("Set", setImpls, c => (c.iterator.take(100000).toList.sorted, c.size))(
    elemSources(_ + 1).map { case (n, s) => ("addAll", n, (t: mu.Set[Int], d: mu.Set[Int]) => t.addAll(s(d)), None) } ++
    elemSources(_ + 1).map { case (n, s) => ("subtractAll", n, (t: mu.Set[Int], d: mu.Set[Int]) => t.subtractAll(s(d)), None) } ++
    List[(String, String, (mu.Set[Int], mu.Set[Int]) => Any, Option[mu.Set[Int] => Any])](
      ("filterInPlace", "p = !self.contains(x+1)", (t, d) => t.filterInPlace(x => !d.contains(x + 1)), None),
      ("filterInPlace", "p = self.size is even", (t, d) => t.filterInPlace(_ => d.size % 2 == 0), None)))

  // ---- buffers
  val bufImpls: List[(String, List[Int] => mu.Buffer[Int])] = List(
    "ArrayBuffer" -> (xs => mu.ArrayBuffer.from(xs)), "ListBuffer" -> (xs => mu.ListBuffer.from(xs)), "ArrayDeque" -> (xs => mu.ArrayDeque.from(xs)),
    "Queue" -> (xs => mu.Queue.from(xs)), "Stack" -> (xs => mu.Stack.from(xs)), "UnrolledBuffer" -> (xs => mu.UnrolledBuffer.from(xs)),
    "ju.ArrayList.asScala" -> (xs => { val l = new java.util.ArrayList[Int]; xs.foreach(l.add); l.asScala }),
    "ju.LinkedList.asScala" -> (xs => { val l = new java.util.LinkedList[Int]; xs.foreach(l.add); l.asScala }))
  type B = mu.Buffer[Int]
  def inPlace(t: B)(ib: mu.IndexedBuffer[Int] => Any, lb: mu.ListBuffer[Int] => Any): Any = t match { case b: mu.IndexedBuffer[Int @unchecked] => ib(b); case b: mu.ListBuffer[Int @unchecked] => lb(b); case _ => throw NA }
  def bufCells = cells[B]("Buffer", bufImpls, c => (c.iterator.take(100000).toList, c.length))(
    seqSources.flatMap { case (n, s) => List[(String, String, (B, B) => Any, Option[B => Any])](
      ("addAll", n, (t, d) => t.addAll(s(d)), None), ("prependAll", n, (t, d) => t.prependAll(s(d)), None), ("insertAll(1, _)", n, (t, d) => t.insertAll(1, s(d)), None),
      ("patchInPlace(1, _, 1)", n, (t, d) => t.patchInPlace(1, s(d), 1), None), ("subtractAll", n, (t, d) => t.subtractAll(s(d)), None),
      ("pushAll (Stack)", n, (t, d) => t match { case st: mu.Stack[Int @unchecked] => st.pushAll(s(d)); case _ => throw NA }, None)) } ++
    List[(String, String, (B, B) => Any, Option[B => Any])](
      ("mapInPlace", "f = x + self.head", (t, d) => inPlace(t)(_.mapInPlace(_ + d.head), _.mapInPlace(_ + d.head)), None),
      ("mapInPlace", "f = x + self.last", (t, d) => inPlace(t)(_.mapInPlace(_ + d.last), _.mapInPlace(_ + d.last)), None),
      ("filterInPlace", "p = !self.contains(x+1)", (t, d) => inPlace(t)(_.filterInPlace(x => !d.contains(x + 1)), _.filterInPlace(x => !d.contains(x + 1))), None),
      ("filterInPlace", "p = self.length is even", (t, d) => inPlace(t)(_.filterInPlace(_ => d.length % 2 == 0), _.filterInPlace(_ => d.length % 2 == 0)), None),
      ("flatMapInPlace", "f = self.view.take(2)", (t, d) => inPlace(t)(_.flatMapInPlace(_ => d.view.take(2)), _.flatMapInPlace(_ => d.view.take(2))), None),
      ("flatMapInPlace", "f = self.iterator.take(2)", (t, d) => inPlace(t)(_.flatMapInPlace(_ => d.iterator.take(2)), _.flatMapInPlace(_ => d.iterator.take(2))), None),
      ("dropWhileInPlace", "p = x < self.last", (t, d) => t.dropWhileInPlace(_ < d.last), None),
      ("takeWhileInPlace", "p = x < self.length", (t, d) => t.takeWhileInPlace(_ < d.length), None)))

  // ---- priority queue and ArraySeq
  def pqCells = cells[mu.PriorityQueue[Int]]("PriorityQueue", List("PriorityQueue" -> (xs => mu.PriorityQueue.from(xs))), c => (c.clone().dequeueAll[Int].take(100000).toList, c.size))(
    elemSources(_ + 1).map { case (n, s) => ("addAll", n, (t: mu.PriorityQueue[Int], d: mu.PriorityQueue[Int]) => t.addAll(s(d)), None) } ++
    List[(String, String, (mu.PriorityQueue[Int], mu.PriorityQueue[Int]) => Any, Option[mu.PriorityQueue[Int] => Any])](
      ("addAll", "reverseIterator", (t, d) => t.addAll(d.reverseIterator), None), ("addAll", "dequeueAll view", (t, d) => t.addAll(d.view.map(_ * 2)), None)))
  def asCells = cells[mu.ArraySeq[Int]]("ArraySeq", List("ArraySeq" -> (xs => mu.ArraySeq.from(xs))), c => c.toList)(List(
    ("mapInPlace", "f = x + self.head", (t, d) => t.mapInPlace(_ + d.head), None), ("mapInPlace", "f = x + self.last", (t, d) => t.mapInPlace(_ + d.last), None),
    ("sortInPlaceBy", "f = self(x % self.length)", (t, d) => t.sortInPlaceBy(x => -d(x % d.length)), None)))

  // ---- maps, keys of type K made from small ints, value = key + 1
  def mapCells[K](fam: String, kf: Int => K, ki: K => Int, impls: List[(String, List[(K, Int)] => mu.Map[K, Int])]): List[Cell] = {
    type M = mu.Map[K, Int]
    val up = (kv: (K, Int)) => (kf(ki(kv._1) + 1), kv._2 + 10)
    val pairSources: List[(String, M => IterableOnce[(K, Int)])] = List(
      "self" -> (d => d), "view" -> (d => d.view), "view.map" -> (d => d.view.map(up)), "view.filter" -> (d => d.view.filter(_._2 % 2 == 1)),
      "view.mapValues" -> (d => d.view.mapValues(_ + 10)), "view.filterKeys" -> (d => d.view.filterKeys(k => ki(k) % 2 == 1)),
      "iterator" -> (d => d.iterator), "iterator.map" -> (d => d.iterator.map(up)), "keysIterator.map" -> (d => d.keysIterator.map(k => (kf(ki(k) + 1), 0))),
      "keySet.view.map" -> (d => d.keySet.view.map(k => (kf(ki(k) + 1), 0))), "values.view.map" -> (d => d.values.view.map(v => (kf(v + 50), v))),
      "to(LazyList)" -> (d => d.to(LazyList)), "toList (control)" -> (d => d.toList))
    val keySources: List[(String, M => IterableOnce[K])] = List(
      "keySet" -> (d => d.keySet), "keys" -> (d => d.keys), "keysIterator" -> (d => d.keysIterator), "keySet.view.filter" -> (d => d.keySet.view.filter(k => ki(k) % 2 == 1)),
      "keySet.view.map(_+1)" -> (d => d.keySet.view.map(k => kf(ki(k) + 1))), "view.map(_._1)" -> (d => d.view.map(_._1)), "values.map(key)" -> (d => d.values.map(kf)),
      "valuesIterator.map(key)" -> (d => d.valuesIterator.map(kf)), "keySet.toList (control)" -> (d => d.keySet.toList))
    def many(t: M) = (0 until 64).foreach(i => t.put(kf(60 + i), i))
    cells[M](fam, impls.map { case (n, mk) => (n, (xs: List[Int]) => mk(xs.map(x => (kf(x), x + 1)))) }, c => (c.iterator.take(100000).toList.map { case (k, v) => (ki(k), v) }.sorted, c.size))(
      pairSources.map { case (n, s) => ("addAll", n, (t: M, d: M) => t.addAll(s(d)), None) } ++
      keySources.map { case (n, s) => ("subtractAll", n, (t: M, d: M) => t.subtractAll(s(d)), None) } ++
      List[(String, String, (M, M) => Any, Option[M => Any])](
        ("filterInPlace", "p = !self.contains(k+1)", (t, d) => t.filterInPlace((k, _) => !d.contains(kf(ki(k) + 1))), None),
        ("filterInPlace", "p = self.size is even", (t, d) => t.filterInPlace((_, _) => d.size % 2 == 0), None),
        ("mapValuesInPlace", "f = self.getOrElse(k+1, 0)", (t, d) => t.mapValuesInPlace((k, _) => d.getOrElse(kf(ki(k) + 1), 0)), None),
        ("getOrElseUpdate(new, _)", "default adds 1 key", (t, _) => t.getOrElseUpdate(kf(50), { t.put(kf(51), 1); 2 }), Some(t => { t.put(kf(51), 1); t.put(kf(50), 2) })),
        ("getOrElseUpdate(new, _)", "default adds 64 keys", (t, _) => t.getOrElseUpdate(kf(50), { many(t); 2 }), Some(t => { many(t); t.put(kf(50), 2) })),
        ("getOrElseUpdate(new, _)", "default removes a key", (t, _) => t.getOrElseUpdate(kf(50), { t.remove(kf(1)); 2 }), Some(t => { t.remove(kf(1)); t.put(kf(50), 2) })),
        ("getOrElseUpdate(new, _)", "default clears", (t, _) => t.getOrElseUpdate(kf(50), { t.clear(); 2 }), Some(t => { t.clear(); t.put(kf(50), 2) })),
        ("getOrElseUpdate(new, _)", "default adds the key", (t, _) => t.getOrElseUpdate(kf(50), { t.put(kf(50), 1); 2 }), Some(t => { t.put(kf(50), 2) })),
        ("updateWith(present)", "f adds 1 key", (t, _) => t.updateWith(kf(1))(_ => { t.put(kf(51), 1); Some(7) }), Some(t => { t.put(kf(51), 1); t.put(kf(1), 7) })),
        ("updateWith(present)", "f adds 64 keys", (t, _) => t.updateWith(kf(1))(_ => { many(t); Some(7) }), Some(t => { many(t); t.put(kf(1), 7) })),
        ("updateWith(present)", "f removes the key", (t, _) => t.updateWith(kf(1))(_ => { t.remove(kf(1)); Some(7) }), Some(t => { t.put(kf(1), 7) })),
        ("updateWith(present)", "f clears", (t, _) => t.updateWith(kf(1))(_ => { t.clear(); Some(7) }), Some(t => { t.clear(); t.put(kf(1), 7) })),
        ("updateWith(present)", "f adds 64 keys, returns None", (t, _) => t.updateWith(kf(1))(_ => { many(t); None }), Some(t => { many(t); t.remove(kf(1)) })),
        ("updateWith(absent)", "f adds 64 keys", (t, _) => t.updateWith(kf(50))(_ => { many(t); Some(7) }), Some(t => { many(t); t.put(kf(50), 7) })),
        ("updateWith(absent)", "f adds the key", (t, _) => t.updateWith(kf(50))(_ => { t.put(kf(50), 1); Some(7) }), Some(t => { t.put(kf(50), 7) }))))
  }
  def ju[K](m: java.util.Map[K, Int]): List[(K, Int)] => mu.Map[K, Int] = xs => { xs.foreach { case (k, v) => m.put(k, v) }; m.asScala }
  def intMaps = mapCells[Int]("Map", identity, identity, List(
    "HashMap" -> (xs => mu.HashMap.from(xs)), "LinkedHashMap" -> (xs => mu.LinkedHashMap.from(xs)), "TreeMap" -> (xs => mu.TreeMap.from(xs)), "ListMap" -> (xs => mu.ListMap.from(xs)),
    "CollisionProofHashMap" -> (xs => mu.CollisionProofHashMap.from(xs)), "OpenHashMap" -> (xs => (mu.OpenHashMap.from(xs): @annotation.nowarn)), "WeakHashMap" -> (xs => mu.WeakHashMap.from(xs)),
    "TrieMap" -> (xs => cc.TrieMap.from(xs)),
    "ju.HashMap.asScala" -> (xs => ju(new java.util.HashMap[Int, Int])(xs)), "ju.TreeMap.asScala" -> (xs => ju(new java.util.TreeMap[Int, Int])(xs)),
    "ju.LinkedHashMap.asScala" -> (xs => ju(new java.util.LinkedHashMap[Int, Int])(xs)),
    "ju.ConcurrentHashMap.asScala" -> (xs => { val m = new java.util.concurrent.ConcurrentHashMap[Int, Int]; xs.foreach { case (k, v) => m.put(k, v) }; m.asScala })))
  def longMaps = mapCells[Long]("Map", _.toLong, _.toInt, List("LongMap" -> (xs => mu.LongMap.from(xs))))
  def refMaps = mapCells[String]("Map", i => f"k$i%03d", _.drop(1).toInt, List("AnyRefMap" -> (xs => (mu.AnyRefMap.from(xs): @annotation.nowarn))))

  lazy val all: Vector[Cell] = (setCells ++ bufCells ++ pqCells ++ asCells ++ intMaps ++ longMaps ++ refMaps).toVector

  def timed(f: () => Any): Either[String, Any] = {
    @volatile var res: Either[String, Any] = Left("HANG")
    val th = new Thread(() => res = try Right(f()) catch { case NA => Left("n/a"); case _: OutOfMemoryError => Left("HANG(heap exhausted)"); case _: StackOverflowError => Left("EXC:StackOverflowError"); case e: Throwable => Left("EXC:" + e.getClass.getSimpleName) })
    th.setDaemon(true); th.start(); th.join(2000); res
  }
  def rank(v: String) = if (v.startsWith("HANG")) 4 else if (v == "WRONG") 3 else if (v.startsWith("EXC")) 2 else if (v == "CME") 1 else 0
  def verdict(c: Cell, init: List[Int]): (String, String) = {
    val (act, exp) = c.run(init)
    val e = timed(exp); if (e == Left("n/a")) return ("n/a", "")
    val a = timed(act)
    def tr(x: Any) = { val s = String.valueOf(x); if (s.length > 160) s.take(160) + "..." else s }
    val v = (e, a) match {
      case (_, Left(h)) if h.startsWith("HANG") => h
      case (x, y) if x == y => "ok"
      case (_, Left("EXC:ConcurrentModificationException")) => "CME"
      case (_, Left(x)) => x
      case (_, Right(_)) => "WRONG"
    }
    (v, if (v == "ok") "" else s"init=${init.size} elements; expected ${tr(e.merge)}; got ${tr(a.merge)}")
  }
  def main(args: Array[String]): Unit = {
    val out = new java.io.FileWriter(args(0), true); var i = args(1).toInt
    // "mark": the previous run died (heap exhausted by a call that never returned) before it could record cell i
    if (args.length > 2 && args(2) == "mark") { val c = all(i); out.write(List(c.fam, c.impl, c.method, c.source, "HANG(heap exhausted)", "the driver ran out of heap").mkString("\t") + "\n"); out.close(); sys.exit(0) }
    while (i < all.length) {
      val c = all(i); var worst = ("ok", "")
      for (init <- List(List(1, 2, 3), (1 to 40).toList) if rank(worst._1) < 4) { val v = verdict(c, init); if (v._1 == "n/a" || rank(v._1) > rank(worst._1)) worst = v }
      out.write(List(c.fam, c.impl, c.method, c.source, worst._1, worst._2).mkString("\t") + "\n"); out.flush()
      if (rank(worst._1) == 4) { out.close(); sys.exit(3) }
      i += 1
    }
    out.close(); sys.exit(0)
  }
}
