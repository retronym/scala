// Map families: mutable LongMap / AnyRefMap / OpenHashMap / CollisionProofHashMap / TrieMap against an
// immutable TreeMap held in a var; immutable IntMap / LongMap / TreeSeqMap / VectorMap / ListMap against a
// Vector of entries in the order the Scaladoc promises (sorted, first insertion, or last modification).
import scala.collection.{immutable => im, mutable => mu, concurrent => co}
import scala.util.Random
import H._

/** A key whose hash codes collide heavily; ordered by its number. */
final class CK(val i: Long) extends Comparable[CK] {
  override def hashCode = if (i % 4 == 0) 7 else (i % 5).toInt
  override def equals(o: Any) = o match { case k: CK => k.i == i; case _ => false }
  override def toString = s"K$i"
  def compareTo(o: CK) = java.lang.Long.compare(i, o.i)
}
object CK { implicit val ord: Ordering[CK] = Ordering.by[CK, Long](_.i) }

final case class Op(n: String, t: Int, a: Long, b: Long, xs: Vector[(Long, Long)]) {
  override def toString = s"$n@$t($a,$b${if (xs.isEmpty) "" else xs.take(12).mkString(",[", ",", if (xs.size > 12) s",..${xs.size}]" else "]")})"
}

object KeyGen {
  /** a key space chosen per program: small dense, medium sparse, or wide with sign and power-of-two keys */
  def space(r: Random): () => Long = r.nextInt(4) match {
    case 0 => () => r.nextInt(12).toLong
    case 1 => () => r.nextInt(300).toLong
    case 2 => () => r.nextInt(5000).toLong - 2500
    case _ => () => r.nextInt(8) match {
      case 0 => 0L; case 1 => -1L; case 2 => (1L << r.nextInt(63)) * (if (r.nextBoolean()) 1 else -1)
      case 3 => r.nextInt(64).toLong << 32; case 4 => Long.MinValue + r.nextInt(3); case 5 => Long.MaxValue - r.nextInt(3)
      case _ => r.nextLong() % 100000
    }
  }
  def ops(r: Random, names: IndexedSeq[(String, Int)], len: Int): Vector[Op] = {
    val key = space(r); val total = names.map(_._2).sum
    Vector.fill(len) {
      var x = r.nextInt(total); var i = 0; while (x >= names(i)._2) { x -= names(i)._2; i += 1 }
      val bulk = r.nextInt(6) match { case 0 => 0; case 1 | 2 => r.nextInt(5); case 3 | 4 => r.nextInt(40); case _ => r.nextInt(400) }
      Op(names(i)._1, r.nextInt(4), key(), r.nextInt(50).toLong, Vector.fill(bulk)((key(), r.nextInt(50).toLong)))
    }
  }
}

class MutMapFam[K](val name: String, key: Long => K, mk: () => mu.Map[K, String], extra: Seq[(String, Int)])(implicit ord: Ordering[K]) extends Fam {
  type P = Op
  final class T(val m: mu.Map[K, String], var mod: im.TreeMap[K, String], val ro: Boolean)
  val common = Vector("put" -> 8, "update" -> 8, "remove" -> 6, "get" -> 3, "apply" -> 2, "getOrElse" -> 2, "contains" -> 2, "getOrElseUpdate" -> 4,
    "updateWith" -> 4, "addAll" -> 6, "addAllMap" -> 2, "subtractAll" -> 3, "+=" -> 3, "-=" -> 3, "filterInPlace" -> 2, "mapValuesInPlace" -> 2,
    "clear" -> 1, "clone" -> 2, "iterate" -> 2, "eq" -> 2, "map" -> 1, "filter" -> 1, "concat" -> 1, "query" -> 2, "iterPartial" -> 2, "keySet" -> 1)
  val names = common ++ extra
  def gen(r: Random) = KeyGen.ops(r, names, 1 + r.nextInt(30))
  def v(b: Long) = "v" + b
  def upf(b: Long)(o: Option[String]): Option[String] = o match { case None => if (b % 3 == 0) None else Some(v(b)); case Some(x) => if (b % 3 == 0) None else Some(x.take(6) + "u") }
  def pred(b: Long)(k: K, s: String) = ((s.hashCode * 31 + b.toInt) & 1) == 0
  def sorted(m: collection.Map[K, String]) = m.toList.sortBy(_._1)
  def run(prog: Vector[Op]): Unit = {
    val ts = mu.ArrayBuffer(new T(mk(), im.TreeMap.empty[K, String], false))
    for (op <- prog) {
      val t = ts(op.t % ts.size); val m = t.m; val k = key(op.a); val kvs = op.xs.map { case (a, b) => (key(a), v(b)) }
      def o = s"${op.n}"
      def mut(f: => Unit): Unit = if (!t.ro) f
      op.n match {
        case "put" => mut(chk(name, o)(t.mod.get(k))({ val r = m.put(k, v(op.b)); r })); if (!t.ro) t.mod += k -> v(op.b)
        case "update" => mut { m.update(k, v(op.b)); t.mod += k -> v(op.b) }
        case "remove" => mut { chk(name, o)(t.mod.get(k))(m.remove(k)); t.mod -= k }
        case "get" => chk(name, o)(t.mod.get(k))(m.get(k))
        case "apply" => chk(name, o)(t.mod(k))(m(k))
        case "getOrElse" => chk(name, o)(t.mod.getOrElse(k, "dflt"))(m.getOrElse(k, "dflt"))
        case "contains" => chk(name, o)(t.mod.contains(k))(m.contains(k))
        case "getOrElseUpdate" => mut { chk(name, o)(t.mod.getOrElse(k, v(op.b)))(m.getOrElseUpdate(k, v(op.b))); if (!t.mod.contains(k)) t.mod += k -> v(op.b) }
        case "updateWith" => mut { val e = upf(op.b)(t.mod.get(k)); chk(name, o)(e)(m.updateWith(k)(upf(op.b))); t.mod = e.fold(t.mod - k)(x => t.mod + (k -> x)) }
        case "addAll" => mut { if (op.b % 2 == 0) m ++= kvs else m.addAll(kvs.iterator); t.mod ++= kvs }
        case "addAllMap" => mut { val src = ts(op.b.toInt % ts.size); if (src ne t) { m ++= src.m; t.mod ++= src.mod } }
        case "subtractAll" => mut { m --= kvs.map(_._1); t.mod --= kvs.map(_._1) }
        case "+=" => mut { m += (k -> v(op.b)); t.mod += k -> v(op.b) }
        case "-=" => mut { m -= k; t.mod -= k }
        case "filterInPlace" => mut { m.filterInPlace(pred(op.b)); t.mod = t.mod.filter { case (a, b) => pred(op.b)(a, b) } }
        case "mapValuesInPlace" => mut { m.mapValuesInPlace((a, s) => s.take(5) + a.hashCode % 3); t.mod = t.mod.map { case (a, s) => (a, s.take(5) + a.hashCode % 3) } }
        case "clear" => mut { m.clear(); t.mod = im.TreeMap.empty }
        case "clone" => ts += new T(m.clone(), t.mod, false)
        case "iterate" =>
          chk(name, o + "/keys")(t.mod.keys.toList)(m.keysIterator.toList.sorted)
          chk(name, o + "/values")(t.mod.values.toList.sorted)(m.valuesIterator.toList.sorted)
          chk(name, o + "/foreachEntry")(t.mod.toList)({ val b = mu.ListBuffer.empty[(K, String)]; m.foreachEntry((a, s) => b += ((a, s))); b.toList.sortBy(_._1) })
          chk(name, o + "/foreach")(t.mod.toList)({ val b = mu.ListBuffer.empty[(K, String)]; m.foreach(b += _); b.toList.sortBy(_._1) })
        case "eq" => chk(name, o)((true, true, t.mod.hashCode))((m == t.mod, t.mod == m, m.hashCode))
        case "map" => chk(name, o)(t.mod.map { case (a, s) => (a, s + "!") }.toList)(sorted(m.map { case (a, s) => (a, s + "!") }))
        case "filter" => chk(name, o)(t.mod.filter { case (a, b) => pred(op.b)(a, b) }.toList)(sorted(m.filter { case (a, b) => pred(op.b)(a, b) }))
        case "concat" => chk(name, o)((t.mod ++ kvs).toList)(sorted(m ++ kvs)); chk(name, o + "/unchanged")(t.mod.toList)(sorted(m))
        case "query" => chk(name, o)((t.mod.size, t.mod.isEmpty, t.mod.count { case (a, b) => pred(op.b)(a, b) }, t.mod.exists(_._1 == k), t.mod.find(_._1 == k)))((m.size, m.isEmpty, m.count { case (a, b) => pred(op.b)(a, b) }, m.exists(_._1 == k), m.find(_._1 == k)))
          check(name, "knownSize", m.knownSize == -1 || m.knownSize == t.mod.size, s"knownSize ${m.knownSize} size ${t.mod.size}")
        case "iterPartial" => val j = (op.b % (t.mod.size + 1)).toInt; val it = m.iterator; val first = it.take(j).toList; val rest = it.toList
          chk(name, o)((t.mod.toList, j))(((first ++ rest).sortBy(_._1), first.size))
        case "keySet" => chk(name, o)((t.mod.keySet.toList, t.mod.keySet.contains(k)))((m.keySet.toList.sorted, m.keySet.contains(k)))
        case other => extraOp(other, op, t.asInstanceOf[T], ts, k, kvs)
      }
      for (t <- ts) { chk(name, op.n + "/content")(t.mod.toList)(sorted(t.m)); chk(name, op.n + "/size")(t.mod.size)(t.m.size) }
    }
  }
  def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: K, kvs: Vector[(K, String)]): Unit = ()
}

class LongMapFam extends MutMapFam[Long]("mutable.LongMap", identity, () => mu.LongMap.empty[String],
  Seq("getOrNull" -> 2, "repack" -> 2, "transformValuesInPlace" -> 2, "mapValuesNow" -> 1, "foreachKV" -> 1, "lmConcat" -> 1, "updated" -> 1, "withDefault" -> 1, "lmMap" -> 1, "fromZip" -> 1)) {
  override def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: Long, kvs: Vector[(Long, String)]): Unit = {
    val m = t.m.asInstanceOf[mu.LongMap[String]]
    n match {
      case "getOrNull" => chk(name, n)(t.mod.getOrElse(k, null))(m.getOrNull(k))
      case "repack" => if (!t.ro) m.repack()
      case "transformValuesInPlace" => m.transformValuesInPlace(_.take(4) + "t"); t.mod = t.mod.map { case (a, s) => (a, s.take(4) + "t") }
      case "mapValuesNow" => chk(name, n)(t.mod.map { case (a, s) => (a, s.length) }.toList)(m.mapValuesNow(_.length).toList.sortBy(_._1))
      case "foreachKV" => chk(name, n)((t.mod.keys.toList, t.mod.values.toList.sorted))({ val a = mu.ListBuffer.empty[Long]; val b = mu.ListBuffer.empty[String]; m.foreachKey(a += _); m.foreachValue(b += _); (a.toList.sorted, b.toList.sorted) })
      case "lmConcat" => val r: mu.LongMap[String] = m.concat(kvs); chk(name, n)((t.mod ++ kvs).toList)(r.toList.sortBy(_._1)); chk(name, n + "/sizeAfterWrite")((t.mod ++ kvs).size + (if ((t.mod ++ kvs).contains(k)) 0 else 1))({ r(k) = "w"; r.size })
      case "updated" => val r = m.updated(k, "x"); chk(name, n)((t.mod + (k -> "x")).toList)(r.toList.sortBy(_._1))
      case "withDefault" => val d = mu.LongMap.withDefault[String](x => "d" + x); d ++= t.mod; chk(name, n)(t.mod.getOrElse(k, "d" + k))(d(k)); chk(name, n + "/get")(t.mod.get(k))(d.get(k))
      case "lmMap" => val r: mu.LongMap[Int] = m.map { case (a, s) => (a / 2, s.length) }; chk(name, n)(t.mod.toList.map { case (a, s) => (a / 2, s.length) }.toMap.keys.toList.sorted)(r.keys.toList.sorted)
      case "fromZip" => val ks = kvs.map(_._1); val vs = kvs.map(_._2); chk(name, n)(im.TreeMap.from(ks.zip(vs)).toList)(mu.LongMap.fromZip(ks.toArray, vs.toArray).toList.sortBy(_._1)); chk(name, n + "/iterable")(im.TreeMap.from(ks.zip(vs)).toList)(mu.LongMap.fromZip(ks, vs).toList.sortBy(_._1))
    }
  }
}

class AnyRefMapFam extends MutMapFam[CK]("mutable.AnyRefMap", new CK(_), () => mu.AnyRefMap.empty[CK, String],
  Seq("getOrNull" -> 2, "repack" -> 2, "transformValuesInPlace" -> 2, "mapValuesNow" -> 1, "foreachKV" -> 1, "arConcat" -> 1, "updated" -> 1, "withDefault" -> 1)) {
  override def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: CK, kvs: Vector[(CK, String)]): Unit = {
    val m = t.m.asInstanceOf[mu.AnyRefMap[CK, String]]
    n match {
      case "getOrNull" => chk(name, n)(t.mod.getOrElse(k, null))(m.getOrNull(k))
      case "repack" => m.repack()
      case "transformValuesInPlace" => m.transformValuesInPlace(_.take(4) + "t"); t.mod = t.mod.map { case (a, s) => (a, s.take(4) + "t") }
      case "mapValuesNow" => chk(name, n)(t.mod.map { case (a, s) => (a, s.length) }.toList)(m.mapValuesNow(_.length).toList.sortBy(_._1))
      case "foreachKV" => chk(name, n)((t.mod.keys.toList, t.mod.values.toList.sorted))({ val a = mu.ListBuffer.empty[CK]; val b = mu.ListBuffer.empty[String]; m.foreachKey(a += _); m.foreachValue(b += _); (a.toList.sorted, b.toList.sorted) })
      case "arConcat" => val r: mu.AnyRefMap[CK, String] = m.concat(kvs); chk(name, n)((t.mod ++ kvs).toList)(r.toList.sortBy(_._1))
      case "updated" => val r = m.updated(k, "x"); chk(name, n)((t.mod + (k -> "x")).toList)(r.toList.sortBy(_._1))
      case "withDefault" => val d = mu.AnyRefMap.withDefault[CK, String](x => "d" + x); d ++= t.mod; chk(name, n)(t.mod.getOrElse(k, "d" + k))(d(k))
    }
  }
}

class TrieMapFam(keyed: String) extends MutMapFam[CK](s"concurrent.TrieMap[$keyed]", if (keyed == "CK") new CK(_) else (l: Long) => new CK(l * 7919), () => co.TrieMap.empty[CK, String],
  Seq("putIfAbsent" -> 4, "replace2" -> 3, "replace3" -> 3, "remove2" -> 3, "snapshot" -> 3, "readOnlySnapshot" -> 3, "snapIter" -> 2, "trieGetOrElseUpdate" -> 2, "filterKeys" -> 1, "lastOption" -> 1)) {
  override def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: CK, kvs: Vector[(CK, String)]): Unit = {
    val m = t.m.asInstanceOf[co.TrieMap[CK, String]]
    def w(f: => Unit) = if (!t.ro) f
    n match {
      case "putIfAbsent" => w { chk(name, n)(t.mod.get(k))(m.putIfAbsent(k, v(op.b))); if (!t.mod.contains(k)) t.mod += k -> v(op.b) }
      case "replace2" => w { chk(name, n)(t.mod.get(k))(m.replace(k, v(op.b))); if (t.mod.contains(k)) t.mod += k -> v(op.b) }
      case "replace3" => w { val old = if (op.b % 2 == 0) t.mod.getOrElse(k, "none") else v(op.b); val ok = t.mod.get(k).contains(old)
        chk(name, n)(ok)(m.replace(k, old, v(op.b + 1))); if (ok) t.mod += k -> v(op.b + 1) }
      case "remove2" => w { val old = if (op.b % 2 == 0) t.mod.getOrElse(k, "none") else v(op.b); val ok = t.mod.get(k).contains(old)
        chk(name, n)(ok)(m.remove(k, old)); if (ok) t.mod -= k }
      case "snapshot" => ts += new T(m.snapshot(), t.mod, false)
      case "readOnlySnapshot" => ts += new T(m.readOnlySnapshot(), t.mod, true)
      case "snapIter" => // an iterator over a snapshot, interleaved with writes to the original
        val s = m.readOnlySnapshot(); val it = s.iterator; val before = t.mod
        val first = it.take((op.b % 5).toInt).toList
        w { m ++= kvs; m -= k; t.mod = (t.mod ++ kvs) - k }
        chk(name, n)(before.toList)((first ++ it.toList).sortBy(_._1))
      case "trieGetOrElseUpdate" => w { var calls = 0; chk(name, n)(t.mod.getOrElse(k, v(op.b)))(m.getOrElseUpdate(k, { calls += 1; v(op.b) })); check(name, n + "/calls", calls == (if (t.mod.contains(k)) 0 else 1), s"calls=$calls"); if (!t.mod.contains(k)) t.mod += k -> v(op.b) }
      case "filterKeys" => chk(name, n)(t.mod.filter(_._1.i % 2 == 0).toList)(m.view.filterKeys(_.i % 2 == 0).toList.sortBy(_._1))
      case "lastOption" => chk(name, n)(t.mod.isEmpty)(m.lastOption.isEmpty); m.lastOption.foreach(kv => check(name, n + "/member", t.mod.get(kv._1).contains(kv._2), kv.toString))
    }
  }
}

class ImmMapFam[K](val name: String, key: Long => K, empty: () => im.Map[K, String], policy: String /* sorted | insertion | modification */, extra: Seq[(String, Int)])(implicit ord: Ordering[K]) extends Fam {
  type P = Op
  final class T(val m: im.Map[K, String], val mod: Vector[(K, String)])
  def upd(mod: Vector[(K, String)], k: K, s: String) = mod.indexWhere(_._1 == k) match {
    case -1 => mod :+ (k -> s)
    case i => if (policy == "modification") mod.patch(i, Nil, 1) :+ (k -> s) else mod.updated(i, k -> s)
  }
  def rem(mod: Vector[(K, String)], k: K) = mod.filter(_._1 != k)
  def ordered(x: Iterable[(K, String)]): List[(K, String)] = if (policy == "sorted") x.toList.sortBy(_._1) else x.toList
  def mo(mod: Vector[(K, String)]) = ordered(mod)
  val names = Vector("updated" -> 10, "removed" -> 6, "+" -> 3, "-" -> 2, "++" -> 4, "++map" -> 2, "removedAll" -> 3, "get" -> 3, "apply" -> 2, "contains" -> 2,
    "getOrElse" -> 1, "updatedWith" -> 3, "filter" -> 2, "map" -> 2, "collect" -> 1, "partition" -> 1, "slice" -> 3, "headLast" -> 2, "tailInit" -> 2,
    "keySet" -> 1, "values" -> 1, "eq" -> 2, "foreachEntry" -> 1, "transform" -> 1, "builder" -> 2, "iterPartial" -> 1, "fromSame" -> 1) ++ extra
  def gen(r: Random) = KeyGen.ops(r, names, 1 + r.nextInt(25))
  def v(b: Long) = "v" + b
  def run(prog: Vector[Op]): Unit = {
    val ts = mu.ArrayBuffer(new T(empty(), Vector.empty))
    for (op <- prog) {
      val t = ts(op.t % ts.size); val m = t.m; val k = key(op.a); val kvs = op.xs.map { case (a, b) => (key(a), v(b)) }
      def add(m2: im.Map[K, String], mod2: Vector[(K, String)]) = ts += new T(m2, mod2)
      val n = op.n
      n match {
        case "updated" => add(m.updated(k, v(op.b)), upd(t.mod, k, v(op.b)))
        case "removed" => add(m.removed(k), rem(t.mod, k))
        case "+" => add(m + (k -> v(op.b)), upd(t.mod, k, v(op.b)))
        case "-" => add(m - k, rem(t.mod, k))
        case "++" => add(m ++ kvs, kvs.foldLeft(t.mod) { case (a, (kk, s)) => upd(a, kk, s) })
        case "++map" => val s = ts(op.b.toInt % ts.size); add(m ++ s.m, ordered(s.mod).foldLeft(t.mod) { case (a, (kk, x)) => upd(a, kk, x) })
        case "removedAll" => add(m.removedAll(kvs.map(_._1)), t.mod.filterNot(e => kvs.exists(_._1 == e._1)))
        case "get" => chk(name, n)(t.mod.find(_._1 == k).map(_._2))(m.get(k))
        case "apply" => chk(name, n)(t.mod.find(_._1 == k).get._2)(m(k))
        case "contains" => chk(name, n)(t.mod.exists(_._1 == k))(m.contains(k))
        case "getOrElse" => chk(name, n)(t.mod.find(_._1 == k).fold("d")(_._2))(m.getOrElse(k, "d"))
        case "updatedWith" => val old = t.mod.find(_._1 == k).map(_._2)
          val f = (o: Option[String]) => o match { case None => if (op.b % 3 == 0) None else Some(v(op.b)); case Some(x) => if (op.b % 3 == 0) None else Some(x.take(5) + "u") }
          add(m.updatedWith(k)(f), f(old).fold(rem(t.mod, k))(x => upd(t.mod, k, x)))
        case "filter" => add(m.filter(e => (e._2.hashCode ^ op.b.toInt) % 2 == 0), t.mod.filter(e => (e._2.hashCode ^ op.b.toInt) % 2 == 0))
        case "map" => chk(name, n)(mo(t.mod.map(e => (e._1, e._2 + "!"))))(ordered(m.map(e => (e._1, e._2 + "!"))))
        case "collect" => chk(name, n)(mo(t.mod.collect { case (a, s) if s.length > 2 => (a, s.length.toString) }))(ordered(m.collect { case (a, s) if s.length > 2 => (a, s.length.toString) }))
        case "partition" => val (a, b) = m.partition(_._2.length > 2); chk(name, n)((mo(t.mod.filter(_._2.length > 2)), mo(t.mod.filterNot(_._2.length > 2))))((ordered(a), ordered(b)))
        case "slice" => if (policy != "sorted" || ord != null) { val i = (op.b % 7).toInt - 1; val j = (op.a.abs % 9).toInt
          val sm = mo(t.mod); val so = mu.ListBuffer.empty[(K, String)]
          if (policy != "sorted" || m.isInstanceOf[collection.SortedMap[_, _]]) {
            chk(name, n)((sm.slice(i, j), sm.take(j), sm.drop(i), sm.takeRight(j), sm.dropRight(j)))((ordered(m.slice(i, j)), ordered(m.take(j)), ordered(m.drop(i)), ordered(m.takeRight(j)), ordered(m.dropRight(j))))
          } else chk(name, n)((sm.slice(i, j).size))(m.slice(i, j).size) }
        case "headLast" => if (policy != "sorted" || m.isInstanceOf[collection.SortedMap[_, _]]) chk(name, n)((mo(t.mod).headOption, mo(t.mod).lastOption))((m.headOption, m.lastOption))
        case "tailInit" => if (t.mod.nonEmpty) { if (policy != "sorted" || m.isInstanceOf[collection.SortedMap[_, _]]) { chk(name, n)((mo(t.mod).tail, mo(t.mod).init))((ordered(m.tail), ordered(m.init))); add(m.tail, mo(t.mod).tail.toVector) } }
        case "keySet" => chk(name, n)((mo(t.mod).map(_._1), t.mod.exists(_._1 == k)))((if (policy == "sorted") m.keySet.toList.sorted else m.keySet.toList, m.keySet.contains(k)))
        case "values" => chk(name, n)(if (policy == "sorted") mo(t.mod).map(_._2).sorted else mo(t.mod).map(_._2))(if (policy == "sorted") m.values.toList.sorted else m.values.toList)
        case "eq" => val ref = im.TreeMap.from(t.mod); chk(name, n)((true, true, ref.hashCode, t.mod.size, t.mod.isEmpty))((m == ref, ref == m, m.hashCode, m.size, m.isEmpty))
        case "foreachEntry" => val b = mu.ListBuffer.empty[(K, String)]; m.foreachEntry((a, s) => b += ((a, s))); chk(name, n)(mo(t.mod))(ordered(b))
        case "transform" => add(m.transform((a, s) => s.take(5) + "t"), t.mod.map(e => (e._1, e._2.take(5) + "t")))
        case "builder" => val b = empty().mapFactory.newBuilder[K, String]; b ++= m; b ++= kvs; val r1 = b.result()
          chk(name, n)(mo(kvs.foldLeft(t.mod) { case (a, (kk, s)) => upd(a, kk, s) }))(ordered(r1))
        case "iterPartial" => val j = (op.b % (t.mod.size + 1)).toInt; val it = m.iterator; val first = it.take(j).toList; val rest = it.toList
          chk(name, n)(mo(t.mod))(ordered(first ++ rest))
        case "fromSame" => chk(name, n)(mo(t.mod))(ordered(m.mapFactory.from(m)))
        case other => extraOp(other, op, t, ts, k, kvs)
      }
      val last = ts.last
      chk(name, n + "/content")(mo(last.mod))(ordered(last.m)); chk(name, n + "/size")(last.mod.size)(last.m.size)
      // persistence: the source version is unchanged
      chk(name, n + "/persist")(mo(t.mod))(ordered(t.m))
    }
  }
  def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: K, kvs: Vector[(K, String)]): Unit = ()
}

/** IntMap / LongMap own methods; keys compared as sets; firstKey/lastKey in unsigned order as documented. */
class PatriciaFam[K](name: String, key: Long => K, empty: () => im.Map[K, String], unsignedLt: (K, K) => Boolean)(implicit ord: Ordering[K])
  extends ImmMapFam[K](name, key, empty, "sorted", Seq("unionWith" -> 3, "intersectionWith" -> 3, "intersection" -> 2, "modifyOrRemove" -> 3, "pUpdateWith" -> 3, "firstLast" -> 3, "pConcat" -> 2, "foreachKV" -> 1)) {
  import scala.collection.immutable.{IntMap, LongMap}
  def unionWith(a: im.Map[K, String], b: im.Map[K, String], f: (K, String, String) => String): im.Map[K, String] = (a, b) match {
    case (x: IntMap[String @unchecked], y: IntMap[String @unchecked]) => x.unionWith[String](y, f.asInstanceOf[(Int, String, String) => String]).asInstanceOf[im.Map[K, String]]
    case (x: LongMap[String @unchecked], y: LongMap[String @unchecked]) => x.unionWith[String](y, f.asInstanceOf[(Long, String, String) => String]).asInstanceOf[im.Map[K, String]]
  }
  def interWith(a: im.Map[K, String], b: im.Map[K, String], f: (K, String, String) => String): im.Map[K, String] = (a, b) match {
    case (x: IntMap[String @unchecked], y: IntMap[String @unchecked]) => x.intersectionWith[String, String](y, f.asInstanceOf[(Int, String, String) => String]).asInstanceOf[im.Map[K, String]]
    case (x: LongMap[String @unchecked], y: LongMap[String @unchecked]) => x.intersectionWith[String, String](y, f.asInstanceOf[(Long, String, String) => String]).asInstanceOf[im.Map[K, String]]
  }
  override def extraOp(n: String, op: Op, t: T, ts: mu.ArrayBuffer[T], k: K, kvs: Vector[(K, String)]): Unit = {
    val m = t.m; val o = ts(op.b.toInt % ts.size)
    val om = o.mod.toMap; val tm = t.mod.toMap
    n match {
      case "unionWith" => val f = (kk: K, a: String, b: String) => (a + b).take(8)
        val exp = (tm.keySet ++ om.keySet).toVector.map(kk => (kk, (tm.get(kk), om.get(kk)) match { case (Some(a), Some(b)) => f(kk, a, b); case (a, b) => a.orElse(b).get }))
        ts += new T(unionWith(m, o.m, f), exp)
      case "intersectionWith" => val f = (kk: K, a: String, b: String) => (b + a).take(8)
        ts += new T(interWith(m, o.m, f), tm.keySet.intersect(om.keySet).toVector.map(kk => (kk, f(kk, tm(kk), om(kk)))))
      case "intersection" => val r = m match { case x: IntMap[String @unchecked] => x.intersection(o.m.asInstanceOf[IntMap[String]]); case x: LongMap[String @unchecked] => x.intersection(o.m.asInstanceOf[LongMap[String]]) }
        ts += new T(r.asInstanceOf[im.Map[K, String]], t.mod.filter(e => om.contains(e._1)))
      case "modifyOrRemove" => val f = (kk: K, s: String) => if ((s.hashCode ^ op.b.toInt) % 3 == 0) None else Some(s.take(4) + "m")
        val r = m match { case x: IntMap[String @unchecked] => x.modifyOrRemove(f.asInstanceOf[(Int, String) => Option[String]]); case x: LongMap[String @unchecked] => x.modifyOrRemove(f.asInstanceOf[(Long, String) => Option[String]]) }
        ts += new T(r.asInstanceOf[im.Map[K, String]], t.mod.flatMap(e => f(e._1, e._2).map(e._1 -> _)))
      case "pUpdateWith" => val f = (a: String, b: String) => (a + "|" + b).take(9)
        val r = m match { case x: IntMap[String @unchecked] => x.updateWith(k.asInstanceOf[Int], v(op.b), f); case x: LongMap[String @unchecked] => x.updateWith(k.asInstanceOf[Long], v(op.b), f) }
        ts += new T(r.asInstanceOf[im.Map[K, String]], upd(t.mod, k, tm.get(k).fold(v(op.b))(a => f(a, v(op.b)))))
      case "firstLast" => val ks = t.mod.map(_._1)
        def fl(x: im.Map[K, String]) = x match { case y: IntMap[_] => (y.firstKey, y.lastKey); case y: LongMap[_] => (y.firstKey, y.lastKey) }
        chk(name, n)(if (ks.isEmpty) throw new NoSuchElementException else (ks.reduce((a, b) => if (unsignedLt(a, b)) a else b), ks.reduce((a, b) => if (unsignedLt(a, b)) b else a)))(fl(m))
      case "pConcat" => val r = (m, o.m) match { case (x: IntMap[String @unchecked], y: IntMap[String @unchecked]) => x ++ y; case (x: LongMap[String @unchecked], y: LongMap[String @unchecked]) => x ++ y }
        ts += new T(r.asInstanceOf[im.Map[K, String]], o.mod.foldLeft(t.mod) { case (a, (kk, s)) => upd(a, kk, s) })
      case "foreachKV" => val a = mu.ListBuffer.empty[Any]; val b = mu.ListBuffer.empty[String]
        m match { case x: IntMap[String @unchecked] => x.foreachKey(a += _); x.foreachValue(b += _); case x: LongMap[String @unchecked] => x.foreachKey(a += _); x.foreachValue(b += _) }
        chk(name, n)((t.mod.map(_._1).sorted.toList, t.mod.map(_._2).sorted.toList))((a.toList.asInstanceOf[List[K]].sorted, b.toList.sorted))
    }
  }
}
