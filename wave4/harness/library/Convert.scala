// scala.collection.convert wrappers in both directions.
// Scala viewed as java.util.*: the same op on the wrapper and on a java.util model with the same contract
// (ArrayList, Arrays.asList for fixed-size, unmodifiable views for read-only, HashSet, TreeMap, ConcurrentHashMap).
// Java viewed as Scala: Buffer / Set programs against an ArrayBuffer / immutable Set in a var; Map programs reuse MutMapFam.
import scala.collection.{immutable => im, mutable => mu, concurrent => co}
import scala.jdk.CollectionConverters._
import scala.util.Random
import java.{util => ju}
import H._

object JFam { val optional = Set[Any]("EX:UnsupportedOperationException", "EX:ClassCastException", "EX:NullPointerException") }

/** A Java-interface program: each op is applied to the wrapper and to the model, results and contents compared. */
abstract class JFam[C] extends Fam {
  type P = Op
  def ops: Vector[(String, Int, (C, Op) => Any)]
  def make(r: Op): (C, C) // under test, model; both built from the op's xs
  def content(c: C): Any
  lazy val names = ops.map(o => (o._1, o._2))
  lazy val byName = ops.map(o => o._1 -> o._3).toMap
  def gen(r: Random) = Op("init", 0, 0, 0, Vector.fill(r.nextInt(4) match { case 0 => 0; case 1 => r.nextInt(5); case _ => r.nextInt(60) })((r.nextInt(40).toLong, r.nextInt(40).toLong))) +: QGen.ops(r, names, 1 + r.nextInt(25))
  def run(prog: Vector[Op]): Unit = {
    val (u, m) = make(prog.head)
    for (op <- prog.tail) {
      val f = byName(op.n)
      // UnsupportedOperationException, ClassCastException and NullPointerException are optional in the java.util contracts:
      // a difference that involves one is accepted, and the content check below still catches any change it hid
      val e = ev(f(m, op)); val a = ev(f(u, op)); checks += 1
      if (e != a) {
        if (JFam.optional(e) || JFam.optional(a)) Excl.hit(s"optional exception in a java.util contract (${kind(e)} vs ${kind(a)})")
        else throw Mismatch(s"$name | ${op.n} | ${kind(e)} -> ${kind(a)}", s"expected ${String.valueOf(e).take(400)} got ${String.valueOf(a).take(400)}")
      }
      chk(name, op.n + "/content")(content(m))(content(u))
    }
  }
}

class JListFam(val name: String, mk: Vector[Int] => ju.List[Integer], model: Vector[Int] => ju.List[Integer]) extends JFam[ju.List[Integer]] {
  type L = ju.List[Integer]
  def make(o: Op) = { val xs = o.xs.map(_._1.toInt); (mk(xs), model(xs)) }
  def content(c: L) = c.asScala.toList
  def i(l: L, o: Op) = (o.b % (l.size + 2)).toInt - (if (o.b % 7 == 0) 1 else 0)
  def e(o: Op): Integer = o.a.toInt
  def jl(o: Op) = new ju.ArrayList[Integer](o.xs.map(x => Integer.valueOf(x._1.toInt)).asJava)
  val ops = Vector[(String, Int, (L, Op) => Any)](
    ("add", 6, (l, o) => l.add(e(o))), ("addAt", 3, (l, o) => l.add(i(l, o), e(o))), ("set", 4, (l, o) => l.set(i(l, o), e(o))),
    ("removeAt", 4, (l, o) => l.remove(i(l, o))), ("removeObj", 3, (l, o) => l.remove(e(o): Object)), ("get", 3, (l, o) => l.get(i(l, o))),
    ("indexOf", 2, (l, o) => (l.indexOf(e(o)), l.lastIndexOf(e(o)), l.contains(e(o)), l.indexOf("x"), l.contains(null))), ("size", 1, (l, o) => (l.size, l.isEmpty)),
    ("iterRemove", 3, (l, o) => { if (name.endsWith("Buffer.asJava") || name.startsWith("ArrayDeque")) { Excl.hit("LM7 Buffer.asJava iterator cannot remove"); "skip" } else { val it = l.iterator; var n = 0; while (it.hasNext) { val x = it.next(); if ((x ^ o.b.toInt) % 3 == 0) { it.remove(); n += 1 } }; n } }),
    ("listIter", 3, (l, o) => { val it = l.listIterator(i(l, o) max 0 min l.size); val b = mu.ListBuffer.empty[Any]
      var k = 0; while (k < 6) { (o.b + k) % 5 match {
        case 0 => if (it.hasNext) b += it.next() else b += "end"
        case 1 => if (it.hasPrevious) b += it.previous() else b += "start"
        case 2 => b += ((it.nextIndex, it.previousIndex))
        case 3 => b += util.Try(it.set(k)).isSuccess
        case _ => b += util.Try(it.add(100 + k)).isSuccess }; k += 1 }; b.toList }),
    ("subList", 3, (l, o) => { val a = (o.b % (l.size + 1)).toInt; val z = a + ((o.a % (l.size - a + 1)).abs.toInt); val s = l.subList(a, z)
      val r = (s.size, s.asScala.toList, s.indexOf(e(o))); if (o.b % 3 == 0) s.clear() else if (o.b % 3 == 1) s.add(e(o)) else if (s.size > 0) s.remove(0); r }),
    ("eq", 2, (l, o) => (l.equals(new ju.ArrayList(l)), new ju.ArrayList(l).equals(l), l.hashCode, l.equals(ju.List.of(1, 2)), l.equals(new ju.LinkedList(l)))),
    ("toArray", 1, (l, o) => (l.toArray.toList, l.toArray(new Array[Integer](0)).toList, l.toArray(new Array[Integer](l.size + 2)).toList)),
    ("containsAll", 1, (l, o) => (l.containsAll(jl(o)), l.containsAll(ju.List.of()))),
    ("addAll", 3, (l, o) => l.addAll(jl(o))), ("addAllAt", 2, (l, o) => l.addAll(i(l, o), jl(o))),
    ("removeAll", 2, (l, o) => l.removeAll(jl(o))), ("retainAll", 2, (l, o) => l.retainAll(jl(o))),
    ("removeIf", 2, (l, o) => l.removeIf(x => x % 4 == o.b % 4)), ("sort", 1, (l, o) => { l.sort(if (o.b % 2 == 0) null else ju.Comparator.reverseOrder[Integer]); () }),
    ("replaceAll", 1, (l, o) => { l.replaceAll(x => x + 1); () }), ("clear", 1, (l, o) => l.clear()), ("forEach", 1, (l, o) => { val b = mu.ListBuffer.empty[Integer]; l.forEach(b += _); b.toList }),
    ("stream", 1, (l, o) => (l.stream.mapToInt(_.intValue).sum, l.spliterator.estimateSize)), ("toString", 1, (l, o) => l.toString)
  )
}

class JSetFam(val name: String, mk: Vector[Int] => ju.Set[Integer], model: Vector[Int] => ju.Set[Integer]) extends JFam[ju.Set[Integer]] {
  type S = ju.Set[Integer]
  def make(o: Op) = { val xs = o.xs.map(_._1.toInt); (mk(xs), model(xs)) }
  def content(c: S) = c.asScala.toList.sorted
  def e(o: Op): Integer = o.a.toInt
  def jl(o: Op) = new ju.ArrayList[Integer](o.xs.map(x => Integer.valueOf(x._1.toInt)).asJava)
  val ops = Vector[(String, Int, (S, Op) => Any)](
    ("add", 6, (s, o) => s.add(e(o))), ("remove", 4, (s, o) => s.remove(e(o))), ("contains", 2, (s, o) => (s.contains(e(o)), s.contains("x"), if (name.contains("BitSet")) { Excl.hit("WL3 null unboxed to 0 by a primitive-keyed asJava view"); false } else s.contains(null))),
    ("size", 1, (s, o) => (s.size, s.isEmpty)),
    ("iterRemove", 3, (s, o) => { val it = s.iterator; var n = 0; while (it.hasNext) { val x = it.next(); if ((x ^ o.b.toInt) % 3 == 0) { it.remove(); n += 1 } }; n }),
    ("eq", 2, (s, o) => (s.equals(new ju.HashSet(s)), new ju.HashSet(s).equals(s), s.hashCode, s.equals(ju.Set.of(1)))),
    ("toArray", 1, (s, o) => (s.toArray.toList.map(_.asInstanceOf[Integer].intValue).sorted, s.toArray(new Array[Integer](0)).toList.map(_.intValue).sorted)),
    ("addAll", 3, (s, o) => s.addAll(jl(o))), ("removeAll", 2, (s, o) => s.removeAll(jl(o))), ("retainAll", 2, (s, o) => s.retainAll(jl(o))), ("containsAll", 1, (s, o) => s.containsAll(jl(o))),
    ("removeIf", 2, (s, o) => s.removeIf(x => x % 4 == o.b % 4)), ("clear", 1, (s, o) => s.clear()), ("stream", 1, (s, o) => s.stream.mapToInt(_.intValue).sum)
  )
}

class JMapFam(val name: String, mk: Vector[(Long, Long)] => ju.Map[CK, String], model: Vector[(Long, Long)] => ju.Map[CK, String]) extends JFam[ju.Map[CK, String]] {
  type M = ju.Map[CK, String]
  def make(o: Op) = (mk(o.xs), model(o.xs))
  def content(c: M) = c.asScala.toList.sortBy(_._1)
  def k(o: Op) = new CK(o.a); def v(o: Op) = "v" + o.b
  def bf(o: Op): java.util.function.BiFunction[CK, String, String] = (a, s) => if (o.b % 4 == 0) null else (if (s == null) "n" else s.take(4)) + a.i % 3
  val ops = Vector[(String, Int, (M, Op) => Any)](
    ("put", 6, (m, o) => m.put(k(o), v(o))), ("get", 3, (m, o) => (m.get(k(o)), m.get("x"), m.getOrDefault(k(o), "d"))), ("remove", 4, (m, o) => m.remove(k(o))),
    ("contains", 2, (m, o) => (m.containsKey(k(o)), m.containsValue(v(o)), m.containsKey("x"))), ("size", 1, (m, o) => (m.size, m.isEmpty)),
    ("putIfAbsent", 3, (m, o) => m.putIfAbsent(k(o), v(o))), ("compute", 3, (m, o) => m.compute(k(o), bf(o))),
    ("computeIfAbsent", 3, (m, o) => m.computeIfAbsent(k(o), a => if (o.b % 4 == 0) null else "c" + a.i)), ("computeIfPresent", 3, (m, o) => m.computeIfPresent(k(o), bf(o))),
    ("merge", 3, (m, o) => m.merge(k(o), v(o), (a, b) => if (o.b % 4 == 0) null else (a + b).take(8))),
    ("replace2", 2, (m, o) => m.replace(k(o), v(o))), ("replace3", 2, (m, o) => m.replace(k(o), if (o.b % 2 == 0) m.getOrDefault(k(o), "none") else v(o), "r")),
    ("remove2", 2, (m, o) => m.remove(k(o), if (o.b % 2 == 0) m.getOrDefault(k(o), "none") else v(o))),
    ("entrySet", 3, (m, o) => { val it = m.entrySet.iterator; val b = mu.ListBuffer.empty[(CK, String)]
      while (it.hasNext) { val en = it.next(); b += ((en.getKey, en.getValue)); (en.getKey.i ^ o.b) % 3 match { case 0 => it.remove(); case 1 => en.setValue(en.getValue + "s"); case _ => } }; b.toList.sortBy(_._1) }),
    ("entryEq", 2, (m, o) => { val es = m.entrySet; (es.size, es.contains(new ju.AbstractMap.SimpleEntry(k(o), v(o))), es.equals(new ju.HashMap(m).entrySet), es.hashCode,
      es.asScala.toList.map(e => (e.getKey, e.getValue, e.hashCode, e.equals(new ju.AbstractMap.SimpleEntry(e.getKey, e.getValue)))).sortBy(_._1)) }),
    ("keySetRemove", 2, (m, o) => (m.keySet.remove(k(o)), m.keySet.contains(k(o)), m.keySet.size)), ("valuesRemove", 2, (m, o) => if (m.values.asScala.count(_ == v(o)) <= 1) m.values.remove(v(o)) else "several"),
    ("keySetIter", 2, (m, o) => { val it = m.keySet.iterator; var n = 0; while (it.hasNext) if (it.next().i % 2 == o.b % 2) { it.remove(); n += 1 }; n }),
    ("eq", 2, (m, o) => (m.equals(new ju.HashMap(m)), new ju.HashMap(m).equals(m), m.hashCode, m.keySet.equals(new ju.HashSet(m.keySet)), m.keySet.hashCode)),
    ("putAll", 2, (m, o) => m.putAll(o.xs.map { case (a, b) => (new CK(a), "v" + b) }.toMap.asJava)), ("clear", 1, (m, o) => m.clear()),
    ("replaceAll", 1, (m, o) => m.replaceAll((a, s) => s.take(3) + "a")), ("forEach", 1, (m, o) => { val b = mu.ListBuffer.empty[(CK, String)]; m.forEach((a, s) => b += ((a, s))); b.toList.sortBy(_._1) }),
    ("values", 1, (m, o) => (m.values.size, m.values.asScala.toList.sorted, m.values.contains(v(o))))
  )
}

/** Java collections viewed as Scala Buffers, against an ArrayBuffer doing the same calls. */
class SBufFam(val name: String, mk: () => mu.Buffer[Int]) extends Fam {
  type P = Op
  val names = Vector("+=" -> 6, "prepend" -> 3, "insert" -> 3, "insertAll" -> 2, "remove" -> 3, "removeN" -> 2, "update" -> 3, "patchInPlace" -> 2, "-=" -> 2, "--=" -> 1, "++=" -> 3,
    "prependAll" -> 2, "clear" -> 1, "dropInPlace" -> 2, "takeInPlace" -> 1, "sliceInPlace" -> 1, "padToInPlace" -> 1, "apply" -> 3,
    "query" -> 2, "iterate" -> 2, "eq" -> 2, "trimEnd" -> 1, "subtractAll" -> 1, "clone" -> 1, "knownSize" -> 1, "dropRightInPlace" -> 1, "dropWhileInPlace" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(30))
  def run(prog: Vector[Op]): Unit = {
    val u = mk(); val m = mu.ArrayBuffer.empty[Int]
    for (op <- prog) {
      val e = op.a.toInt; val xs = op.xs.map(_._1.toInt); val i = (op.b % (m.size + 2)).toInt - (if (op.b % 9 == 0) 1 else 0); val n = op.n
      def both(f: mu.Buffer[Int] => Any) = { val ee = ev(f(m)); val aa = ev(f(u)); checks += 1
        if (ee != aa) {
          if (kind(ee) != "val" && kind(aa) != "val") Excl.hit("exception type of an out-of-range Buffer index")
          else if (n == "removeN" && op.b.toInt % 4 == 0) Excl.hit("Buffer.remove(i, 0) with i out of range")
          else throw Mismatch(s"$name | $n | ${kind(ee)} -> ${kind(aa)}", s"expected $ee got $aa")
        } }
      n match {
        case "+=" => both(_ += e); case "prepend" => both(_.prepend(e)); case "insert" => both(_.insert(i, e)); case "insertAll" => both(_.insertAll(i, xs))
        case "remove" => both(_.remove(i)); case "removeN" => both(_.remove(i, op.b.toInt % 4)); case "update" => both(_.update(i, e))
        case "patchInPlace" => both(_.patchInPlace(i, xs, op.b.toInt % 5)); case "-=" => both(_ -= e); case "--=" => both(_ --= xs); case "++=" => both(_ ++= xs)
        case "prependAll" => both(_.prependAll(xs)); case "clear" => both(_.clear());
        case "dropInPlace" => both(_.dropInPlace(i)); case "takeInPlace" => both(_.takeInPlace(i))
        case "sliceInPlace" => both(_.sliceInPlace(i, i + op.b.toInt % 6)); case "padToInPlace" => both(_.padToInPlace(i + 3, -1)); case "apply" => both(_(i))
        case "query" => both(b => (b.length, b.indexOf(e), b.lastIndexOf(e), b.contains(e), b.headOption, b.lastOption, b.sum, b.isEmpty))
        case "iterate" => both(b => (b.iterator.toList, b.reverseIterator.toList, b.view.slice(1, 4).toList, b.sorted, b.distinct))
        case "eq" => both(b => (b == m, b.hashCode, b.toSeq == List.from(m)))
        case "trimEnd" => both(_.trimEnd(op.b.toInt % 4)); case "subtractAll" => both(_.subtractAll(xs))
        case "clone" => both(_.clone().toList); case "knownSize" => check(name, n, u.knownSize == -1 || u.knownSize == m.size, s"${u.knownSize} vs ${m.size}")
        case "dropRightInPlace" => both(_.dropRightInPlace(i)); case "dropWhileInPlace" => both(_.dropWhileInPlace(_ % 4 != 0))
      }
      chk(name, n + "/content")(m.toList)(u.toList)
    }
  }
}

/** Java sets viewed as Scala mutable Sets, against an immutable Set in a var. */
class SSetFam(val name: String, mk: () => mu.Set[Int]) extends Fam {
  type P = Op
  val names = Vector("add" -> 6, "remove" -> 4, "+=" -> 3, "-=" -> 3, "++=" -> 3, "--=" -> 2, "filterInPlace" -> 2, "contains" -> 3, "query" -> 2, "iterate" -> 2, "eq" -> 2,
    "clear" -> 1, "map" -> 1, "diff" -> 1, "subsetOf" -> 1, "update" -> 2, "clone" -> 1, "knownSize" -> 1, "iterPartial" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(30))
  def run(prog: Vector[Op]): Unit = {
    val u = mk(); var m = im.TreeSet.empty[Int]
    for (op <- prog) {
      val e = op.a.toInt; val xs = op.xs.map(_._1.toInt); val n = op.n
      n match {
        case "add" => chk(name, n)(!m(e))(u.add(e)); m += e
        case "remove" => chk(name, n)(m(e))(u.remove(e)); m -= e
        case "+=" => u += e; m += e; case "-=" => u -= e; m -= e; case "++=" => u ++= xs; m ++= xs; case "--=" => u --= xs; m --= xs
        case "filterInPlace" => u.filterInPlace(_ % 3 != op.b % 3); m = m.filter(_ % 3 != op.b % 3)
        case "contains" => chk(name, n)((m(e), m.contains(e)))((u(e), u.contains(e)))
        case "query" => chk(name, n)((m.size, m.isEmpty, m.sum, m.count(_ > e)))((u.size, u.isEmpty, u.sum, u.count(_ > e)))
        case "iterate" => chk(name, n)(m.toList)(u.iterator.toList.sorted)
        case "eq" => chk(name, n)((true, true, m.hashCode))((u == m, m == u, u.hashCode))
        case "clear" => u.clear(); m = im.TreeSet.empty
        case "map" => chk(name, n)(m.map(_ / 2).toList)(u.map(_ / 2).toList.sorted)
        case "diff" => chk(name, n)((m.diff(xs.toSet).toList, m.intersect(xs.toSet).toList))((u.diff(xs.toSet).toList.sorted, u.intersect(xs.toSet).toList.sorted))
        case "subsetOf" => chk(name, n)((m.subsetOf(xs.toSet), xs.toSet.subsetOf(m)))((u.subsetOf(xs.toSet), xs.toSet.subsetOf(u)))
        case "update" => u.update(e, op.b % 2 == 0); m = if (op.b % 2 == 0) m + e else m - e
        case "clone" => chk(name, n)(m.toList)(u.clone().toList.sorted)
        case "knownSize" => check(name, n, u.knownSize == -1 || u.knownSize == m.size, s"${u.knownSize} vs ${m.size}")
        case "iterPartial" => val it = u.iterator; val a = it.take(op.b.toInt % 4).toList; chk(name, n)(m.toList)((a ++ it).sorted)
      }
      chk(name, n + "/content")(m.toList)(u.toList.sorted)
    }
  }
}

/** concurrent.Map views of ConcurrentHashMap / ConcurrentSkipListMap: the ConcurrentMap-specific methods. */
class SCMapFam(name: String, mk: () => co.Map[CK, String]) extends MutMapFam[CK](name, new CK(_), mk, Seq("putIfAbsent" -> 4, "replace2" -> 3, "replace3" -> 3, "remove2" -> 3)) {
  override def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: CK, kvs: Vector[(CK, String)]): Unit = {
    val m = t.m.asInstanceOf[co.Map[CK, String]]
    n match {
      case "putIfAbsent" => chk(name, n)(t.mod.get(k))(m.putIfAbsent(k, v(op.b))); if (!t.mod.contains(k)) t.mod += k -> v(op.b)
      case "replace2" => chk(name, n)(t.mod.get(k))(m.replace(k, v(op.b))); if (t.mod.contains(k)) t.mod += k -> v(op.b)
      case "replace3" => val old = if (op.b % 2 == 0) t.mod.getOrElse(k, "none") else v(op.b); val ok = t.mod.get(k).contains(old)
        chk(name, n)(ok)(m.replace(k, old, v(op.b + 1))); if (ok) t.mod += k -> v(op.b + 1)
      case "remove2" => val old = if (op.b % 2 == 0) t.mod.getOrElse(k, "none") else v(op.b); val ok = t.mod.get(k).contains(old)
        chk(name, n)(ok)(m.remove(k, old)); if (ok) t.mod -= k
    }
  }
}

object ConvertFams {
  def jint(xs: Vector[Int]) = xs.map(Integer.valueOf)
  def al(xs: Vector[Int]) = new ju.ArrayList[Integer](jint(xs).asJava)
  def ckm(xs: Vector[(Long, Long)]) = xs.map { case (a, b) => (new CK(a), "v" + b) }
  def all: Seq[Fam] = Seq(
    new JListFam("ArrayBuffer.asJava", xs => mu.ArrayBuffer.from(jint(xs)).asJava, al),
    new JListFam("ListBuffer.asJava", xs => mu.ListBuffer.from(jint(xs)).asJava, al),
    new JListFam("ArrayDeque.asJava", xs => mu.ArrayDeque.from(jint(xs)).asJava, al),
    new JListFam("Vector.asJava", xs => jint(xs).asJava, xs => ju.Collections.unmodifiableList(al(xs))),
    new JListFam("List.asJava", xs => jint(xs).toList.asJava, xs => ju.Collections.unmodifiableList(al(xs))),
    new JListFam("mutable.ArraySeq.asJava", xs => mu.ArraySeq.from(jint(xs)).asJava, xs => ju.Arrays.asList(jint(xs).toArray: _*)),
    new JListFam("ArrayList.asScala.asJava(roundtrip)", xs => al(xs).asScala.asJava, al),
    new JSetFam("mutable.HashSet.asJava", xs => mu.HashSet.from(jint(xs)).asJava, xs => new ju.HashSet(al(xs))),
    new JSetFam("mutable.TreeSet.asJava", xs => mu.TreeSet.from(jint(xs)).asJava, xs => new ju.HashSet(al(xs))),
    new JSetFam("mutable.BitSet.asJava", xs => mu.BitSet.fromSpecific(xs.map(_.abs)).asInstanceOf[mu.Set[Int]].asInstanceOf[mu.Set[Integer]].asJava, xs => new ju.HashSet(al(xs.map(_.abs)))),
    new JSetFam("immutable.Set.asJava", xs => jint(xs).toSet.asJava, xs => ju.Collections.unmodifiableSet(new ju.HashSet(al(xs)))),
    new JMapFam("mutable.HashMap.asJava", xs => mu.HashMap.from(ckm(xs)).asJava, xs => new ju.TreeMap(ckm(xs).toMap.asJava)),
    new JMapFam("mutable.LinkedHashMap.asJava", xs => mu.LinkedHashMap.from(ckm(xs)).asJava, xs => new ju.TreeMap(ckm(xs).toMap.asJava)),
    new JMapFam("mutable.TreeMap.asJava", xs => mu.TreeMap.from(ckm(xs)).asJava, xs => new ju.TreeMap(ckm(xs).toMap.asJava)),
    new JMapFam("mutable.LongMap-like AnyRefMap.asJava", xs => mu.AnyRefMap.from(ckm(xs)).asJava, xs => new ju.TreeMap(ckm(xs).toMap.asJava)),
    new JMapFam("TrieMap.asJava", xs => co.TrieMap.from(ckm(xs)).asJava, xs => new ju.concurrent.ConcurrentHashMap(ckm(xs).toMap.asJava)),
    new JMapFam("immutable.Map.asJava", xs => ckm(xs).toMap.asJava, xs => ju.Collections.unmodifiableMap(new ju.TreeMap(ckm(xs).toMap.asJava))),
    new SBufFam("ArrayList.asScala", () => new ju.ArrayList[Int]().asScala),
    new SBufFam("LinkedList.asScala", () => new ju.LinkedList[Int]().asScala),
    new SSetFam("HashSet.asScala", () => new ju.HashSet[Int]().asScala),
    new SSetFam("TreeSet.asScala", () => new ju.TreeSet[Int]().asScala),
    new MutMapFam[CK]("HashMap.asScala", new CK(_), () => new ju.HashMap[CK, String]().asScala, Nil),
    new MutMapFam[CK]("TreeMap.asScala", new CK(_), () => new ju.TreeMap[CK, String]().asScala, Nil),
    new MutMapFam[CK]("LinkedHashMap.asScala", new CK(_), () => new ju.LinkedHashMap[CK, String]().asScala, Nil),
    new SCMapFam("ConcurrentHashMap.asScala", () => new ju.concurrent.ConcurrentHashMap[CK, String]().asScala),
    new SCMapFam("ConcurrentSkipListMap.asScala", () => new ju.concurrent.ConcurrentSkipListMap[CK, String]().asScala)
  )
}
