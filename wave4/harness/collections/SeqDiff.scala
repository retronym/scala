// N-version differential test of the Seq API: one random call, every implementation, List as reference.
import scala.collection.{immutable => im, mutable => mu}
import scala.util.Random
import java.util.concurrent._

object SeqDiff {
  final case class Args(i: Int, j: Int, k: Int, x: Int, n: Int, other: List[Int])
  type S = scala.collection.Seq[Int]
  final case class Op(name: String, small: Boolean, f: (S, Args, List[Int] => S) => Any)
  def op(name: String)(f: (S, Args, List[Int] => S) => Any) = Op(name, false, f)
  def sop(name: String)(f: (S, Args, List[Int] => S) => Any) = Op(name, true, f)

  val ops: Vector[Op] = Vector(
    op("take")((s, a, _) => s.take(a.i)), op("drop")((s, a, _) => s.drop(a.i)),
    op("takeRight")((s, a, _) => s.takeRight(a.i)), op("dropRight")((s, a, _) => s.dropRight(a.i)),
    op("slice")((s, a, _) => s.slice(a.i, a.j)), op("splitAt")((s, a, _) => s.splitAt(a.i)),
    op("patch")((s, a, mk) => s.patch(a.i, mk(a.other), a.j)),
    op("patch.iter")((s, a, mk) => s.patch(a.i, mk(a.other).iterator, a.j)),
    op("updated")((s, a, _) => s.updated(a.i, a.x)),
    op("padTo")((s, a, _) => s.padTo(a.n, a.x)),
    op("indexOf")((s, a, _) => s.indexOf(a.x, a.i)), op("lastIndexOf")((s, a, _) => s.lastIndexOf(a.x, a.i)),
    op("indexOfSlice")((s, a, mk) => s.indexOfSlice(mk(a.other), a.i)),
    op("lastIndexOfSlice")((s, a, mk) => s.lastIndexOfSlice(mk(a.other), a.i)),
    op("containsSlice")((s, a, mk) => s.containsSlice(mk(a.other))),
    op("segmentLength")((s, a, _) => s.segmentLength(_ > a.x, a.i)),
    op("indexWhere")((s, a, _) => s.indexWhere(_ > a.x, a.i)), op("lastIndexWhere")((s, a, _) => s.lastIndexWhere(_ > a.x, a.i)),
    op("startsWith")((s, a, mk) => s.startsWith(mk(a.other), a.i)), op("endsWith")((s, a, mk) => s.endsWith(mk(a.other))),
    op("sameElements")((s, a, mk) => s.sameElements(mk(a.other))),
    op("corresponds")((s, a, mk) => s.corresponds(mk(a.other))(_ <= _)),
    op("sorted")((s, _, _) => s.sorted), op("sortBy")((s, _, _) => s.sortBy(-_)), op("sortWith")((s, _, _) => s.sortWith(_ > _)),
    op("distinct")((s, _, _) => s.distinct), op("distinctBy")((s, a, _) => s.distinctBy(_ % 3)),
    op("reverse")((s, _, _) => s.reverse), op("reverseIterator")((s, _, _) => s.reverseIterator),
    op("diff")((s, a, mk) => s.diff(mk(a.other))), op("intersect")((s, a, mk) => s.intersect(mk(a.other))),
    op("zip")((s, a, mk) => s.zip(mk(a.other))), op("zipAll")((s, a, mk) => s.zipAll(mk(a.other), -1, -2)),
    op("zipWithIndex")((s, _, _) => s.zipWithIndex), op("lazyZip")((s, a, mk) => s.lazyZip(mk(a.other)).map(_ + _)),
    op("prepended")((s, a, _) => s.prepended(a.x)), op("appended")((s, a, _) => s.appended(a.x)),
    op("prependedAll")((s, a, mk) => s.prependedAll(mk(a.other))), op("appendedAll")((s, a, mk) => s.appendedAll(mk(a.other))),
    op("concat.iter")((s, a, mk) => s.concat(mk(a.other).iterator)), op("concat.view")((s, a, mk) => s ++ mk(a.other).view.map(_ + 1)),
    op("span")((s, a, _) => s.span(_ > a.x)), op("partition")((s, a, _) => s.partition(_ > a.x)),
    op("takeWhile")((s, a, _) => s.takeWhile(_ > a.x)), op("dropWhile")((s, a, _) => s.dropWhile(_ > a.x)),
    op("filter")((s, a, _) => s.filter(_ > a.x)), op("filterNot")((s, a, _) => s.filterNot(_ > a.x)),
    op("map")((s, _, _) => s.map(_ * 2)), op("flatMap")((s, a, mk) => s.flatMap(e => if (e > a.x) mk(a.other.take(2)) else Nil)),
    op("collect")((s, a, _) => s.collect { case e if e > a.x => e.toString }), op("collectFirst")((s, a, _) => s.collectFirst { case e if e > a.x => e }),
    op("scanLeft")((s, _, _) => s.scanLeft(0)(_ - _)), op("scanRight")((s, _, _) => s.scanRight(0)(_ - _)),
    op("foldLeft")((s, _, _) => s.foldLeft(1)(_ * 31 + _)), op("foldRight")((s, _, _) => s.foldRight(1)(_ * 31 + _)),
    op("reduceLeftOption")((s, _, _) => s.reduceLeftOption(_ - _)), op("reduceRightOption")((s, _, _) => s.reduceRightOption(_ - _)),
    op("reduceRight")((s, _, _) => s.reduceRight(_ - _)), op("reduceLeft")((s, _, _) => s.reduceLeft(_ - _)),
    op("min")((s, _, _) => s.min), op("maxBy")((s, _, _) => s.maxBy(_ % 3)), op("minByOption")((s, _, _) => s.minByOption(_ % 3)),
    op("sum")((s, _, _) => s.sum), op("product")((s, _, _) => s.product), op("count")((s, a, _) => s.count(_ > a.x)),
    op("lengthCompare")((s, a, _) => Integer.signum(s.lengthCompare(a.i))), op("sizeCompare.it")((s, a, mk) => Integer.signum(s.sizeCompare(mk(a.other)))),
    op("lengthIs")((s, a, _) => (s.lengthIs > a.i, s.lengthIs == a.i, s.sizeIs <= a.i)),
    op("isDefinedAt")((s, a, _) => s.isDefinedAt(a.i)), op("lift")((s, a, _) => s.lift(a.i)), op("applyOrElse")((s, a, _) => s.applyOrElse(a.i, (_: Int) => -99)),
    op("apply")((s, a, _) => s(a.i)), op("head")((s, _, _) => s.head), op("last")((s, _, _) => s.last),
    op("init")((s, _, _) => s.init), op("tail")((s, _, _) => s.tail), op("headOption")((s, _, _) => (s.headOption, s.lastOption)),
    sop("inits")((s, _, _) => s.inits), sop("tails")((s, _, _) => s.tails),
    op("sliding")((s, a, _) => s.sliding(a.n, a.k)), op("grouped")((s, a, _) => s.grouped(a.n)),
    op("sliding.x")((s, a, _) => { val it = s.sliding(a.i, a.j); (it.hasNext, it.take(3).toList) }),
    op("grouped.x")((s, a, _) => { val it = s.grouped(a.i); (it.hasNext, it.take(3).toList) }),
    sop("combinations")((s, a, _) => s.take(8).combinations(a.n % 5)), sop("permutations")((s, _, _) => s.take(5).permutations),
    op("groupBy")((s, _, _) => s.groupBy(_ % 3)), op("groupMap")((s, _, _) => s.groupMap(_ % 3)(_ + 1)), op("groupMapReduce")((s, _, _) => s.groupMapReduce(_ % 3)(_ + 1)(_ - _)),
    op("copyToArray")((s, a, _) => { val arr = Array.fill(a.n min 50)(-7); val r = s.copyToArray(arr, a.i, a.j); (r, arr) }),
    op("copyToArray2")((s, a, _) => { val arr = Array.fill(a.n min 50)(-7); val r = s.copyToArray(arr, a.i); (r, arr) }),
    op("toArray")((s, _, _) => s.toArray), op("toVector")((s, _, _) => s.toVector), op("toIndexedSeq")((s, _, _) => s.toIndexedSeq), op("toBuffer")((s, _, _) => s.toBuffer),
    op("hashCode")((s, _, _) => s.hashCode), op("equals")((s, a, mk) => (s == mk(a.other), mk(a.other) == s, s == s.toList, s.toList == s)),
    op("canEqual")((s, a, mk) => s.equals(mk(a.other).view) ),
    op("indices")((s, _, _) => s.indices), op("search")((s, a, _) => s.distinct.sorted.search(a.x, a.i, a.j)),
    op("search2")((s, a, _) => s.distinct.sorted.search(a.x)), op("unzip")((s, _, _) => s.map(e => (e, -e)).unzip), op("unzip3")((s, _, _) => s.map(e => (e, -e, e)).unzip3),
    op("tapEach")((s, _, _) => { var c = 0; val r = s.tapEach(c += _); (r.toList, c) }),
    op("find")((s, a, _) => s.find(_ > a.x)), op("findLast")((s, a, _) => s.findLast(_ > a.x)), op("exists")((s, a, _) => (s.exists(_ > a.x), s.forall(_ > a.x), s.contains(a.x))),
    op("mkString")((s, _, _) => s.mkString("[", ",", "]")), op("addString")((s, _, _) => s.addString(new StringBuilder, ";").toString),
    op("transpose")((s, a, _) => s.map(e => List(e, a.x)).transpose), op("flatten")((s, a, mk) => s.map(e => mk(a.other.take(e.abs % 3))).flatten),
    op("partitionMap")((s, a, _) => s.partitionMap(e => if (e > a.x) Left(e) else Right(-e))),
    op("splitAt2")((s, a, _) => { val (l, r) = s.splitAt(a.i); (l.length, r.length) }),
    op("isEmpty")((s, _, _) => (s.isEmpty, s.nonEmpty, s.size, s.length, s.knownSize match { case -1 => s.length; case n => n })),
    // iterator forms
    op("it.slice")((s, a, _) => s.iterator.slice(a.i, a.j)), op("it.drop.take")((s, a, _) => s.iterator.drop(a.i).take(a.j)),
    op("it.take.drop")((s, a, _) => s.iterator.take(a.i).drop(a.j)), op("it.drop.drop")((s, a, _) => s.iterator.drop(a.i).drop(a.j)),
    op("it.slice.slice")((s, a, _) => s.iterator.slice(a.i, a.j).slice(a.k, a.n)),
    op("it.patch")((s, a, mk) => s.iterator.patch(a.i, mk(a.other).iterator, a.j)), op("it.padTo")((s, a, _) => s.iterator.padTo(a.n, a.x)),
    op("it.sliding")((s, a, _) => s.iterator.sliding(a.n, a.k).withPartial(a.x > 2)), op("it.grouped.pad")((s, a, _) => s.iterator.grouped(a.n).withPadding(-5)),
    op("it.indexOf")((s, a, _) => s.iterator.indexOf(a.x, a.i)), op("it.indexWhere")((s, a, _) => s.iterator.indexWhere(_ > a.x, a.i)),
    op("it.knownSize")((s, a, _) => { val it = s.iterator.drop(a.i).take(a.j); val ks = it.knownSize; val n = it.size; (ks == -1 || ks == n, n) }),
    op("it.knownSize2")((s, a, _) => { val it = s.iterator.slice(a.i, a.j); val ks = it.knownSize; val n = it.size; (ks == -1 || ks == n, n) }),
    op("it.exhaust")((s, a, _) => { val it = s.iterator.drop(a.i); while (it.hasNext) it.next(); (it.hasNext, try it.next() catch { case e: NoSuchElementException => "NSE" }) }),
    op("it.span")((s, a, _) => { val (l, r) = s.iterator.span(_ > a.x); val rr = r.toList; (l.toList, rr) }),
    op("it.duplicate")((s, a, _) => { val (l, r) = s.iterator.duplicate; (l.drop(a.i).toList, r.take(a.j).toList) }),
    op("it.buffered")((s, a, _) => { val b = s.iterator.buffered; (b.headOption, b.drop(a.i).toList) }),
    op("it.copyToArray")((s, a, _) => { val arr = Array.fill(a.n min 50)(-7); val it = s.iterator; val r = it.copyToArray(arr, a.i, a.j); (r, arr) }),
    op("it.sameElements")((s, a, mk) => s.iterator.sameElements(mk(a.other))), op("it.zipAll")((s, a, mk) => s.iterator.zipAll(mk(a.other).iterator, -1, -2)),
    op("it.distinct")((s, a, _) => s.iterator.distinctBy(_ % 4)), op("it.scanLeft")((s, a, _) => s.iterator.scanLeft(0)(_ + _).drop(a.i)),
    op("revit.drop")((s, a, _) => s.reverseIterator.drop(a.i).take(a.j)), op("revit.slice")((s, a, _) => s.reverseIterator.slice(a.i, a.j)),
    op("revit.knownSize")((s, a, _) => { val it = s.reverseIterator.drop(a.i); val ks = it.knownSize; val n = it.size; (ks == -1 || ks == n, n) }),
    // view forms
    op("v.slice")((s, a, _) => s.view.slice(a.i, a.j)), op("v.drop.take")((s, a, _) => s.view.drop(a.i).take(a.j)),
    op("v.takeRight")((s, a, _) => s.view.takeRight(a.i)), op("v.dropRight")((s, a, _) => s.view.dropRight(a.i)),
    op("v.reverse.slice")((s, a, _) => s.view.reverse.slice(a.i, a.j)), op("v.patch")((s, a, mk) => s.view.patch(a.i, mk(a.other), a.j)),
    op("v.padTo")((s, a, _) => s.view.padTo(a.n, a.x)), op("v.updated")((s, a, _) => s.view.updated(a.i, a.x)),
    op("v.appended.apply")((s, a, _) => s.view.appended(a.x).prepended(a.k).apply(a.i)), op("v.concat.apply")((s, a, mk) => s.view.concat(mk(a.other)).apply(a.i)),
    op("v.drop.apply")((s, a, _) => s.view.drop(a.i).apply(a.j)), op("v.take.apply")((s, a, _) => s.view.take(a.i).apply(a.j)),
    op("v.slice.len")((s, a, _) => { val v = s.view.slice(a.i, a.j); (v.size, v.knownSize match { case -1 => v.size; case n => n }, v.toList) }),
    op("v.dropRight.len")((s, a, _) => { val v = s.view.dropRight(a.i).takeRight(a.j); (v.size, v.toList) }),
    op("v.sorted")((s, a, _) => s.view.sorted.slice(a.i, a.j)), op("v.distinct")((s, _, _) => s.view.distinct),
    op("v.zip.rev")((s, a, mk) => s.view.reverse.zip(mk(a.other))), op("v.map.revit")((s, a, _) => s.view.map(_ + 1).reverseIterator.drop(a.i)),
    op("v.tapEach")((s, a, _) => { var c = 0; val v = s.view.tapEach(c += _).take(a.i); val l = v.toList; (l, c >= l.sum.min(0)) }),
    op("v.sliding")((s, a, _) => s.view.sliding(a.n, a.k)), op("v.scanLeft")((s, a, _) => s.view.scanLeft(0)(_ + _).drop(a.i)),
    op("v.indexOfSlice")((s, a, mk) => s.view.indexOfSlice(mk(a.other), a.i)), op("v.lastIndexOf")((s, a, _) => s.view.lastIndexOf(a.x, a.i)),
    op("v.rev.it.knownSize")((s, a, _) => { val it = s.view.reverse.iterator.drop(a.i); val ks = it.knownSize; val n = it.size; (ks == -1 || ks == n, n) }),
    op("v.it.knownSize")((s, a, _) => { val it = s.view.slice(a.i, a.j).iterator.drop(a.k); val ks = it.knownSize; val n = it.size; (ks == -1 || ks == n, n) }),
    op("stepper")((s, a, _) => { val st = s.stepper; val b = List.newBuilder[Int]; var c = a.i & 7; while (c > 0 && st.hasStep) { b += st.nextStep(); c -= 1 }; val sp = st.trySplit(); val est = st.estimateSize; val rest = st.iterator.toList; (b.result(), (if (sp == null) Nil else sp.iterator.toList) ::: rest, est == rest.size || est == Long.MaxValue) }),
  )

  def norm(a: Any): Any = a match {
    case it: Iterator[_] => it.take(100000).map(norm).toList
    case arr: Array[_] => arr.toList.map(norm)
    case m: scala.collection.Map[_, _] => m.iterator.map(norm).toSet
    case s: scala.collection.Set[_] => s.iterator.map(norm).toSet
    case it: Iterable[_] => it.iterator.take(100000).map(norm).toList
    case p: Product => p.productPrefix :: p.productIterator.map(norm).toList
    case x => x
  }

  // implementations, including differently shaped instances of the same class
  val impls: Vector[(String, List[Int] => S)] = Vector(
    "List" -> (xs => xs),
    "Vector" -> (xs => Vector.from(xs)),
    "Vector.prep" -> (xs => xs.foldRight(Vector.empty[Int])(_ +: _)),
    "Vector.sliced" -> (xs => (Vector.fill(40)(9) ++ xs ++ Vector.fill(1100)(9)).slice(40, 40 + xs.length)),
    "Vector.cat" -> (xs => { val (a, b) = xs.splitAt(xs.length / 3); Vector.from(a) ++ Vector.from(b) }),
    "im.ArraySeq.ofInt" -> (xs => im.ArraySeq.from(xs)),
    "im.ArraySeq.ofRef" -> (xs => im.ArraySeq.untagged.from(xs)),
    "LazyList" -> (xs => LazyList.from(xs)),
    "LazyList.forced" -> (xs => LazyList.from(xs).force),
    "LazyList.cons" -> (xs => xs.foldRight(LazyList.empty[Int])(_ #:: _)),
    "Queue" -> (xs => im.Queue.from(xs)),
    "Queue.split" -> (xs => { val (a, b) = xs.splitAt(xs.length / 2); b.foldLeft(im.Queue.from(a))(_ enqueue _) }),
    "Queue.deq" -> (xs => (im.Queue(7) ++ xs.take(1)).enqueueAll(xs.drop(1)).dequeue._2),
    "ArrayBuffer" -> (xs => mu.ArrayBuffer.from(xs)),
    "ListBuffer" -> (xs => mu.ListBuffer.from(xs)),
    "ArrayDeque" -> (xs => mu.ArrayDeque.from(xs)),
    "ArrayDeque.wrapped" -> (xs => { val d = new mu.ArrayDeque[Int](16); (1 to 10).foreach(d += _); (1 to 10).foreach(_ => d.removeHead()); val (a, b) = xs.splitAt(xs.length / 2); d ++= b; a.reverseIterator.foreach(d.prepend); d }),
    "mu.Queue" -> (xs => mu.Queue.from(xs)),
    "mu.Stack" -> (xs => mu.Stack.from(xs)),
    "mu.ArraySeq" -> (xs => mu.ArraySeq.from(xs)),
    "UnrolledBuffer" -> (xs => mu.UnrolledBuffer.from(xs)),
    "wrappedArray" -> (xs => xs.toArray.toSeq),
    "mu.wrappedArray" -> (xs => scala.Predef.wrapIntArray(xs.toArray)),
    "juArrayList" -> (xs => { val l = new java.util.ArrayList[Int]; xs.foreach(l.add); scala.jdk.CollectionConverters.ListHasAsScala(l).asScala }),
    "juLinkedList" -> (xs => { val l = new java.util.LinkedList[Int]; xs.foreach(l.add); scala.jdk.CollectionConverters.ListHasAsScala(l).asScala }),
    "Vector.view.toIndexedSeq" -> (xs => Vector.from(xs).view.toIndexedSeq),
    "List.view.toSeq" -> (xs => xs.view.map(identity).toSeq),
    "Stream" -> (xs => (im.Stream.from(xs): @annotation.nowarn)),
    "ArraySeq.rev" -> (xs => im.ArraySeq.from(xs.reverse).reverse),
    "Vector.rev" -> (xs => Vector.from(xs.reverse).reverse),
    "Vector.updated" -> (xs => if (xs.isEmpty) Vector.empty else Vector.from(xs).updated(xs.length / 2, xs(xs.length / 2)).appended(1).init),
  )

  val sizes = Array(0, 1, 2, 3, 4, 5, 7, 8, 15, 16, 17, 31, 32, 33, 63, 64, 65, 100, 1023, 1024, 1025, 1057, 2080)
  def genList(r: Random, small: Boolean): List[Int] = {
    val n = if (small) r.nextInt(7) else r.nextInt(10) match { case 0 | 1 => sizes(r.nextInt(sizes.length)); case 2 => r.nextInt(70); case _ => r.nextInt(9) }
    List.fill(n)(r.nextInt(9) - 2)
  }
  def idx(r: Random, len: Int): Int = r.nextInt(16) match {
    case 0 => -1; case 1 => 0; case 2 => 1; case 3 => len - 1; case 4 => len; case 5 => len + 1
    case 6 => Int.MinValue; case 7 => Int.MaxValue; case 8 => Int.MaxValue - 1; case 9 => Int.MinValue + 1
    case 10 => len / 2; case 11 => -len; case 12 => Int.MaxValue - len; case _ => r.nextInt(len + 3)
  }
  def genArgs(r: Random, xs: List[Int], small: Boolean): Args = {
    val len = xs.length
    val other = r.nextInt(6) match { case 0 => xs; case 1 => xs.slice(r.nextInt(len + 1), len); case 2 => Nil; case 3 => xs.take(r.nextInt(len + 1)); case _ => genList(r, true) }
    val n = r.nextInt(10) match { case 0 => -1; case 1 => 0; case 2 => Int.MinValue; case 3 => len; case 4 => len + 1; case _ => 1 + r.nextInt(6) }
    Args(idx(r, len), idx(r, len), r.nextInt(8) match { case 0 => 0; case 1 => -1; case 2 => Int.MaxValue; case _ => 1 + r.nextInt(4) }, r.nextInt(9) - 2, n, other)
  }

  def run(o: Op, mk: List[Int] => S, xs: List[Int], a: Args): Any =
    try norm(o.f(mk(xs), a, mk)) catch { case _: StackOverflowError => "EX:StackOverflowError"; case _: IndexOutOfBoundsException => "EX:IndexOutOfBoundsException"; case e: Throwable => "EX:" + e.getClass.getSimpleName }

  def kind(ref: Any, got: Any): String = {
    def ex(a: Any) = a match { case s: String if s.startsWith("EX:") => true; case _ => false }
    if (ex(ref) && ex(got)) "exc-type" else if (ex(ref)) "value-for-exc" else if (ex(got)) "exc-for-value" else "value"
  }

  def main(argv: Array[String]): Unit = {
    val seed = argv(0).toLong; val budgetMs = argv(1).toLong * 1000; val only = argv.drop(2).toSet
    val r = new Random(seed)
    val pool = Executors.newCachedThreadPool((rn: Runnable) => { val t = new Thread(rn); t.setDaemon(true); t })
    def timed[T](f: => T): Option[T] = { val fu = pool.submit(new Callable[T] { def call = f }); try Some(fu.get(20, TimeUnit.SECONDS)) catch { case _: TimeoutException => fu.cancel(true); None } }
    val seen = mu.LinkedHashMap.empty[String, (Int, String)]
    val t0 = System.currentTimeMillis; var cases = 0L
    val useOps = if (only.isEmpty) ops else ops.filter(o => only(o.name))
    while (System.currentTimeMillis - t0 < budgetMs) {
      val o = useOps(r.nextInt(useOps.length))
      val xs = genList(r, o.small); val a = genArgs(r, xs, o.small)
      cases += 1
      val ref = timed(run(o, impls(0)._2, xs, a))
      if (ref.isEmpty) println(s"HANG ref ${o.name} $xs $a")
      else for ((name, mk) <- impls.tail) {
        timed(run(o, mk, xs, a)) match {
          case None => val sig = s"${o.name} | $name | hang"; if (!seen.contains(sig)) { seen(sig) = (1, s"xs=$xs $a"); println(s"NEW $sig\n  xs=$xs $a") }
          case Some(got) if got != ref.get =>
            val sig = s"${o.name} | $name | ${kind(ref.get, got)}"
            seen.get(sig) match {
              case Some((c, ex)) => seen(sig) = (c + 1, ex)
              case None =>
                // shrink: fewer elements, simpler args, same signature
                var cx = xs; var ca = a
                def bad(x: List[Int], aa: Args) = { val rr = run(o, impls(0)._2, x, aa); val g = run(o, mk, x, aa); g != rr && kind(rr, g) == kind(ref.get, got) }
                if (xs.length <= 200) {
                  var progress = true
                  while (progress) {
                    progress = false
                    val cands = (cx.indices.map(i => (cx.patch(i, Nil, 1), ca)) ++ ca.other.indices.map(i => (cx, ca.copy(other = ca.other.patch(i, Nil, 1)))) ++
                      Seq((cx.map(_ => 0), ca), (cx, ca.copy(other = Nil)), (cx, ca.copy(x = 0)), (cx, ca.copy(k = 1)), (cx, ca.copy(n = 1)), (cx, ca.copy(i = 0)), (cx, ca.copy(j = 0)))).filter(_ != ((cx, ca)))
                    cands.find { case (x, aa) => timed(bad(x, aa)).getOrElse(false) }.foreach { case (x, aa) => cx = x; ca = aa; progress = true }
                  }
                }
                val rr = run(o, impls(0)._2, cx, ca); val g = run(o, mk, cx, ca)
                def cut(s: Any) = { val t = s.toString; if (t.length > 300) t.take(300) + "..." else t }
                val desc = s"xs=${cut(cx)} (len ${cx.length}) $ca\n    List: ${cut(rr)}\n    $name: ${cut(g)}"
                seen(sig) = (1, desc); if (!sig.endsWith("exc-type")) println(s"NEW $sig\n  $desc")
            }
          case _ =>
        }
      }
    }
    println(s"\n==== seed $seed: $cases cases x ${impls.length - 1} impls, ${ops.length} ops, ${seen.size} signatures")
    seen.toList.sortBy(_._1).foreach { case (sig, (c, d)) => println(f"$c%7d  $sig" + (if (sig.endsWith("exc-type")) "   " + d.linesIterator.toList.tail.map(_.trim).mkString(" / ") + " @ " + d.linesIterator.next().take(120) else "")) }
    System.exit(0)
  }
}
