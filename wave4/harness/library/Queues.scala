// PriorityQueue against a sorted list; mutable Stack and Queue against a List in a var;
// immutable Queue (several versions kept, to check persistence) against a List.
import scala.collection.{immutable => im, mutable => mu}
import scala.util.Random
import H._

object QGen {
  def ops(r: Random, names: IndexedSeq[(String, Int)], len: Int): Vector[Op] = {
    val total = names.map(_._2).sum; val vr = 1 + r.nextInt(if (r.nextBoolean()) 10 else 1000)
    Vector.fill(len) {
      var x = r.nextInt(total); var i = 0; while (x >= names(i)._2) { x -= names(i)._2; i += 1 }
      val bulk = r.nextInt(6) match { case 0 => 0; case 1 | 2 => r.nextInt(5); case 3 | 4 => r.nextInt(40); case _ => r.nextInt(300) }
      Op(names(i)._1, r.nextInt(4), r.nextInt(vr).toLong, r.nextInt(50).toLong, Vector.fill(bulk)((r.nextInt(vr).toLong, 0L)))
    }
  }
}

class PQFam extends Fam {
  type P = Op
  val name = "mutable.PriorityQueue"
  final class T(val q: mu.PriorityQueue[Int], var mod: List[Int], val ord: Ordering[Int]) { def sortedMod = mod.sorted(ord.reverse) }
  val names = Vector("enqueue" -> 8, "+=" -> 6, "++=" -> 4, "dequeue" -> 8, "dequeueAll" -> 2, "head" -> 3, "clone" -> 2, "reverse" -> 2, "mapInPlace" -> 2,
    "clear" -> 1, "iterate" -> 2, "reverseIterator" -> 1, "toQueue" -> 1, "query" -> 2, "map" -> 1, "filter" -> 1, "from" -> 1, "copyToArray" -> 1,
    "drop" -> 1, "builder" -> 1, "dequeuePartial" -> 2, "takeWhile" -> 1, "sizeAfter" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(30))
  def ms(x: Iterable[Int]) = x.toList.sorted
  def run(prog: Vector[Op]): Unit = {
    val ts = mu.ArrayBuffer(new T(mu.PriorityQueue.empty[Int], Nil, Ordering.Int))
    for (op <- prog) {
      val t = ts(op.t % ts.size); val q = t.q; val e = op.a.toInt; val xs = op.xs.map(_._1.toInt); val n = op.n
      n match {
        case "enqueue" => q.enqueue(xs: _*); t.mod ++= xs
        case "+=" => q += e; t.mod ::= e
        case "++=" => if (op.b % 2 == 0) q ++= xs else q ++= xs.iterator; t.mod ++= xs
        case "dequeue" => chk(name, n)(t.sortedMod.head)(q.dequeue()); if (t.mod.nonEmpty) t.mod = t.sortedMod.tail
        case "dequeueAll" => chk(name, n)(t.sortedMod)(q.dequeueAll[Int]); t.mod = Nil
        case "head" => chk(name, n)(t.sortedMod.head)(q.head)
        case "clone" => ts += new T(q.clone(), t.mod, t.ord)
        case "reverse" => ts += new T(q.reverse, t.mod, t.ord.reverse)
        case "mapInPlace" => q.mapInPlace(x => (x * 7 + op.b.toInt) % 101); t.mod = t.mod.map(x => (x * 7 + op.b.toInt) % 101)
        case "clear" => q.clear(); t.mod = Nil
        case "iterate" => chk(name, n)((ms(t.mod), ms(t.mod), ms(t.mod)))((ms(q.iterator.toList), ms(q.toList), ms(q)))
        case "reverseIterator" => chk(name, n)(ms(t.mod))(ms(q.reverseIterator.toList))
          check(name, n + "/isReverseOfIterator", q.reverseIterator.toList == q.iterator.toList.reverse, s"${q.iterator.toList}")
        case "toQueue" => chk(name, n)(ms(t.mod))(ms(q.toQueue))
        case "query" => chk(name, n)((t.mod.size, t.mod.isEmpty, t.mod.contains(e), t.mod.sum, t.mod.count(_ > e)))((q.size, q.isEmpty, q.exists(_ == e), q.sum, q.count(_ > e)))
          check(name, "knownSize", q.knownSize == -1 || q.knownSize == t.mod.size, s"${q.knownSize}")
        case "map" => chk(name, n)(ms(t.mod.map(_ % 13)))(ms(q.map(_ % 13)))
        case "filter" => val r = q.filter(_ % 2 == 0); chk(name, n)(t.sortedMod.filter(_ % 2 == 0))(r.dequeueAll[Int])
        case "from" => val r = mu.PriorityQueue.from(xs)(t.ord); chk(name, n)(xs.toList.sorted(t.ord.reverse))(r.dequeueAll[Int])
        case "copyToArray" => val a = Array.fill(t.mod.size + 2)(-1); val c = q.copyToArray(a, 1); chk(name, n)((t.mod.size, ms(t.mod)))((c, ms(a.slice(1, 1 + c))))
        case "drop" => val j = (op.b % 5).toInt; chk(name, n)(t.mod.size - j max 0)(q.drop(j).size)
        case "builder" => val b = mu.PriorityQueue.newBuilder[Int](t.ord); b ++= xs; b += e; chk(name, n)((e :: xs.toList).sorted(t.ord.reverse))(b.result().dequeueAll[Int])
        case "dequeuePartial" => val j = (op.b % 6).toInt; val r = List.fill(j min t.mod.size)(q.dequeue()); chk(name, n)(t.sortedMod.take(j))(r); t.mod = t.sortedMod.drop(j)
        case "takeWhile" => chk(name, n)(true)(q.takeWhile(_ => true).size == t.mod.size)
        case "sizeAfter" => chk(name, n)(t.mod.size)(q.length)
      }
      for (t <- ts) { chk(name, n + "/content")(ms(t.mod))(ms(t.q)); chk(name, n + "/size")(t.mod.size)(t.q.size)
        chk(name, n + "/headIsMax")(t.sortedMod.headOption)(t.q.headOption) }
    }
  }
}

class StackQueueFam extends Fam {
  type P = Op
  val name = "mutable.Stack/Queue"
  // two structures: a Stack (model head = top) and a Queue (model head = front)
  val names = Vector("push" -> 6, "push2" -> 2, "pushAll" -> 4, "pop" -> 5, "popAll" -> 1, "popWhile" -> 2, "top" -> 2, "enqueue" -> 6, "enqueue2" -> 2, "enqueueAll" -> 4, "dequeue" -> 5,
    "dequeueFirst" -> 2, "dequeueAll" -> 2, "dequeueWhile" -> 2, "front" -> 2, "sApply" -> 2, "qUpdate" -> 2, "sUpdate" -> 2, "clone" -> 2, "iterate" -> 2, "removeHeadLast" -> 2,
    "prepend" -> 2, "insert" -> 2, "removeAt" -> 2, "clearAndShrink" -> 1, "reverse" -> 1, "sliding" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(30))
  def run(prog: Vector[Op]): Unit = {
    var s = mu.Stack.empty[Int]; var sm = List.empty[Int]
    var q = mu.Queue.empty[Int]; var qm = List.empty[Int]
    for (op <- prog) {
      val e = op.a.toInt; val xs = op.xs.map(_._1.toInt).toList; val n = op.n; val i = (op.b % 8).toInt
      n match {
        case "push" => s.push(e); sm ::= e
        case "push2" => s.push(e, e + 1, e + 2); sm = List(e + 2, e + 1, e) ::: sm
        case "pushAll" => s.pushAll(xs); sm = xs.reverse ::: sm
        case "pop" => chk(name, n)(sm.head)(s.pop()); if (sm.nonEmpty) sm = sm.tail
        case "popAll" => chk(name, n)(sm)(s.popAll()); sm = Nil
        case "popWhile" => chk(name, n)(sm.takeWhile(_ % 3 != 0))(s.popWhile(_ % 3 != 0)); sm = sm.dropWhile(_ % 3 != 0)
        case "top" => chk(name, n)(sm.head)(s.top)
        case "enqueue" => q.enqueue(e); qm :+= e
        case "enqueue2" => q.enqueue(e, e + 1, e + 2); qm ++= List(e, e + 1, e + 2)
        case "enqueueAll" => q.enqueueAll(xs); qm ++= xs
        case "dequeue" => chk(name, n)(qm.head)(q.dequeue()); if (qm.nonEmpty) qm = qm.tail
        case "dequeueFirst" => chk(name, n)(qm.find(_ % 3 == 0))(q.dequeueFirst(_ % 3 == 0)); qm.indexWhere(_ % 3 == 0) match { case -1 => case j => qm = qm.patch(j, Nil, 1) }
        case "dequeueAll" => chk(name, n)(qm.filter(_ % 4 == 1))(q.dequeueAll(_ % 4 == 1)); qm = qm.filterNot(_ % 4 == 1)
        case "dequeueWhile" => chk(name, n)(qm.takeWhile(_ % 5 != 0))(q.dequeueWhile(_ % 5 != 0)); qm = qm.dropWhile(_ % 5 != 0)
        case "front" => chk(name, n)(qm.head)(q.front)
        case "sApply" => chk(name, n)(sm(i))(s(i))
        case "qUpdate" => chk(name, n)(if (i < qm.size) "ok" else throw new IndexOutOfBoundsException)({ q(i) = e; "ok" }); if (i < qm.size) qm = qm.updated(i, e)
        case "sUpdate" => chk(name, n)(if (i < sm.size) "ok" else throw new IndexOutOfBoundsException)({ s(i) = e; "ok" }); if (i < sm.size) sm = sm.updated(i, e)
        case "clone" => val s2 = s.clone(); val q2 = q.clone(); chk(name, n)((sm, qm))((s2.toList, q2.toList)); if (op.b % 2 == 0) { s2.push(1); q2.enqueue(1) } ; chk(name, n + "/independent")((sm, qm))((s.toList, q.toList)); if (op.b % 4 == 0) { s = s2; q = q2; sm = 1 :: sm; qm :+= 1 }
        case "iterate" => chk(name, n)((sm, qm, sm.reverse, qm.reverse, sm.headOption, qm.lastOption))((s.iterator.toList, q.iterator.toList, s.reverseIterator.toList, q.reverseIterator.toList, s.headOption, q.lastOption))
        case "removeHeadLast" => chk(name, n)((qm.headOption, sm.lastOption))((q.removeHeadOption(), s.removeLastOption())); qm = qm.drop(1); sm = sm.dropRight(1)
        case "prepend" => q.prepend(e); qm ::= e; s.append(e); sm :+= e
        case "insert" => val j = i min qm.size; q.insert(j, e); qm = qm.patch(j, List(e), 0); val js = i min sm.size; s.insertAll(js, xs); sm = sm.patch(js, xs, 0)
        case "removeAt" => if (i < qm.size) { chk(name, n)(qm(i))(q.remove(i)); qm = qm.patch(i, Nil, 1) }; if (i < sm.size) { val c = 1 min (sm.size - i); s.remove(i, c); sm = sm.patch(i, Nil, c) }
        case "clearAndShrink" => s.clearAndShrink(i); sm = Nil; q.clearAndShrink(i); qm = Nil
        case "reverse" => chk(name, n)((sm.reverse, qm.reverse))((s.reverse.toList, q.reverse.toList))
        case "sliding" => chk(name, n)((sm.sliding(2 + i % 3).toList, qm.grouped(1 + i).toList))((s.sliding(2 + i % 3).map(_.toList).toList, q.grouped(1 + i).map(_.toList).toList))
      }
      chk(name, n + "/stack")(sm)(s.toList); chk(name, n + "/queue")(qm)(q.toList); chk(name, n + "/sizes")((sm.size, qm.size, sm.lastOption, qm.lastOption))((s.size, q.size, s.lastOption, q.lastOption))
    }
  }
}

class ImmQueueFam extends Fam {
  type P = Op
  val name = "immutable.Queue"
  val names = Vector("enqueue" -> 8, "enqueueAll" -> 4, "dequeue" -> 6, "dequeueOption" -> 2, "front" -> 2, "prepended" -> 3, "appended" -> 3, "tail" -> 3, "init" -> 2,
    "apply" -> 2, "updated" -> 2, "reverse" -> 2, "concat" -> 2, "filter" -> 2, "take" -> 2, "drop" -> 2, "eq" -> 2, "last" -> 2, "iterate" -> 2, "map" -> 1, "patch" -> 1, "from" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(30))
  def run(prog: Vector[Op]): Unit = {
    val ts = mu.ArrayBuffer[(im.Queue[Int], List[Int])]((im.Queue.empty, Nil))
    for (op <- prog) {
      val (q, m) = ts(op.t % ts.size); val e = op.a.toInt; val xs = op.xs.map(_._1.toInt).toList; val n = op.n; val i = (op.b % 9).toInt
      def add(q2: im.Queue[Int], m2: List[Int]) = ts += ((q2, m2))
      n match {
        case "enqueue" => add(q.enqueue(e), m :+ e)
        case "enqueueAll" => add(q.enqueueAll(xs), m ++ xs)
        case "dequeue" => if (m.nonEmpty) { val (h, r) = q.dequeue; chk(name, n)(m.head)(h); add(r, m.tail) } else chk(name, n)(throw new NoSuchElementException)(q.dequeue)
        case "dequeueOption" => chk(name, n)(m.headOption.map(h => (h, m.tail)))(q.dequeueOption)
        case "front" => chk(name, n)(m.head)(q.front)
        case "prepended" => add(q.prepended(e), e :: m)
        case "appended" => add(q.appended(e), m :+ e)
        case "tail" => if (m.nonEmpty) add(q.tail, m.tail)
        case "init" => if (m.nonEmpty) add(q.init, m.init)
        case "apply" => chk(name, n)(m(i))(q(i))
        case "updated" => if (i < m.size) add(q.updated(i, e), m.updated(i, e))
        case "reverse" => add(q.reverse, m.reverse)
        case "concat" => val (q2, m2) = ts(op.b.toInt % ts.size); add(q ++ q2, m ++ m2)
        case "filter" => add(q.filter(_ % 3 != i % 3), m.filter(_ % 3 != i % 3))
        case "take" => add(q.take(i), m.take(i)); add(q.takeRight(i), m.takeRight(i))
        case "drop" => add(q.drop(i), m.drop(i)); add(q.dropRight(i), m.dropRight(i))
        case "eq" => chk(name, n)((true, m.hashCode, true))((q == m, q.hashCode, im.Queue.from(m) == q))
        case "last" => chk(name, n)((m.lastOption, m.headOption, m.length, m.isEmpty))((q.lastOption, q.headOption, q.length, q.isEmpty))
        case "iterate" => chk(name, n)((m, m.reverse, m.indexOf(e), m.contains(e)))((q.iterator.toList, q.reverseIterator.toList, q.indexOf(e), q.contains(e)))
        case "map" => add(q.map(_ + 1), m.map(_ + 1))
        case "patch" => add(q.patch(i, xs, 2), m.patch(i, xs, 2))
        case "from" => add(im.Queue.from(xs), xs)
      }
      val (lq, lm) = ts.last; chk(name, n + "/content")(lm)(lq.toList); chk(name, n + "/persist")(m)(q.toList)
    }
  }
}
