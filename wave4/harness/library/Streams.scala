// Steppers, Java streams and accumulators.
// Stepper oracle: drained after a random tree of trySplit calls (some taken mid-way), every element of the
// collection appears exactly once; with ORDERED the leaves read in split order give the iterator's order;
// SIZED means estimateSize is exact, SUBSIZED that both halves of a split are SIZED; DISTINCT and SORTED hold.
// Streams: sequential and parallel streams against the iterator. Accumulators: programs against ArrayBuffer.
import scala.collection.{immutable => im, mutable => mu, Stepper}
import scala.jdk.StreamConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.{Accumulator, IntAccumulator, LongAccumulator, DoubleAccumulator, AnyAccumulator}
import scala.util.Random
import java.util.Spliterator._
import H._

final case class SCase(kind: Int, xs: Vector[Int], seed: Long) { override def toString = s"kind=${StepperFam.kinds(kind)._1} n=${xs.size} xs=${xs.take(20).mkString(",")}${if (xs.size > 20) ",.." else ""} seed=$seed" }

object StepperFam {
  type K = (String, Vector[Int] => (List[Any], () => Stepper[_]))
  def str(xs: Vector[Int]) = xs.map(x => ('a' + (x % 26)).toChar).mkString
  val kinds: Vector[K] = Vector(
    ("List", xs => (xs.toList, () => xs.toList.stepper)), ("Vector", xs => (xs.toList, () => xs.stepper)), ("Array[Int]", xs => (xs.toList, () => xs.toArray.stepper)),
    ("Array[String]", xs => (xs.map("s" + _).toList, () => xs.map("s" + _).toArray.stepper)), ("Array[Double]", xs => (xs.map(_.toDouble).toList, () => xs.map(_.toDouble).toArray.stepper)),
    ("Array[Long]", xs => (xs.map(_.toLong).toList, () => xs.map(_.toLong).toArray.stepper)), ("Array[Byte]", xs => (xs.map(_.toByte.toInt).toList, () => xs.map(_.toByte).toArray.stepper)),
    ("Array[Char]", xs => (xs.map(_.toChar.toInt).toList, () => xs.map(_.toChar).toArray.stepper)), ("Array[Short]", xs => (xs.map(_.toShort.toInt).toList, () => xs.map(_.toShort).toArray.stepper)),
    ("Array[Float]", xs => (xs.map(_.toFloat.toDouble).toList, () => xs.map(_.toFloat).toArray.stepper)),
    ("immutable.ArraySeq", xs => (xs.toList, () => im.ArraySeq.from(xs).stepper)), ("mutable.ArraySeq", xs => (xs.toList, () => mu.ArraySeq.from(xs).stepper)),
    ("Range", xs => { val r = xs.size until (3 * xs.size + 1) by 2; (r.toList, () => r.stepper) }), ("Range.inclusive desc", xs => { val r = xs.size to -xs.size by -3; (r.toList, () => r.stepper) }),
    ("NumericRange[Long]", xs => { val r = 5L until (5L + 2 * xs.size) by 2L; (r.toList, () => r.stepper) }),
    ("immutable.HashSet", xs => { val s = im.HashSet.from(xs); (s.toList, () => s.stepper) }), ("immutable.HashMap.keyStepper", xs => { val s = xs.map(x => x -> x.toString).toMap; (s.keys.toList, () => s.keyStepper) }),
    ("immutable.HashMap.valueStepper", xs => { val s = im.HashMap.from(xs.map(x => x -> x * 2)); (s.values.toList, () => s.valueStepper) }), ("immutable.HashMap.stepper", xs => { val s = im.HashMap.from(xs.map(x => x -> x)); (s.toList, () => s.stepper) }),
    ("immutable.TreeSet", xs => { val s = im.TreeSet.from(xs); (s.toList, () => s.stepper) }), ("immutable.TreeMap.keyStepper", xs => { val s = im.TreeMap.from(xs.map(x => x -> x)); (s.keys.toList, () => s.keyStepper) }),
    ("immutable.TreeMap.valueStepper", xs => { val s = im.TreeMap.from(xs.map(x => x -> -x)); (s.values.toList, () => s.valueStepper) }),
    ("immutable.BitSet", xs => { val s = im.BitSet.fromSpecific(xs); (s.toList, () => s.stepper) }), ("mutable.BitSet", xs => { val s = mu.BitSet.fromSpecific(xs.map(_ * 37)); (s.toList, () => s.stepper) }),
    ("ArrayBuffer", xs => (xs.toList, () => mu.ArrayBuffer.from(xs).stepper)), ("ArrayDeque", xs => { val d = mu.ArrayDeque.from(xs); d.prependAll(xs.take(5)); d.dropInPlace(3); (d.toList, () => d.stepper) }),
    ("ListBuffer", xs => (xs.toList, () => mu.ListBuffer.from(xs).stepper)), ("mutable.HashSet", xs => { val s = mu.HashSet.from(xs); (s.toList, () => s.stepper) }),
    ("mutable.HashMap.keyStepper", xs => { val s = mu.HashMap.from(xs.map(x => x -> x)); (s.keys.toList, () => s.keyStepper) }), ("mutable.HashMap.valueStepper", xs => { val s = mu.HashMap.from(xs.map(x => x -> x)); (s.values.toList, () => s.valueStepper) }),
    ("mutable.LinkedHashMap.keyStepper", xs => { val s = mu.LinkedHashMap.from(xs.map(x => x -> x)); (s.keys.toList, () => s.keyStepper) }), ("mutable.LinkedHashSet", xs => { val s = mu.LinkedHashSet.from(xs); (s.toList, () => s.stepper) }),
    ("mutable.TreeSet", xs => { val s = mu.TreeSet.from(xs); (s.toList, () => s.stepper) }), ("LazyList", xs => (xs.toList, () => LazyList.from(xs).stepper)), ("Iterator", xs => (xs.toList, () => xs.iterator.stepper)),
    ("String.stepper", xs => { val s = str(xs); (s.toList.map(_.toInt), () => s.stepper) }), ("String.charStepper", xs => { val s = str(xs); (s.toList.map(_.toInt), () => s.charStepper) }),
    ("String.codePointStepper", xs => { val s = xs.map(x => if (x % 5 == 0) "😀" else ('a' + x % 26).toChar.toString).mkString; (s.codePoints.toArray.toList, () => s.codePointStepper) }),
    ("IntAccumulator", xs => { val a = IntAccumulator.fromSpecific(xs); (xs.toList, () => a.stepper) }), ("LongAccumulator", xs => { val a = LongAccumulator.fromSpecific(xs.map(_.toLong)); (xs.map(_.toLong).toList, () => a.stepper) }),
    ("DoubleAccumulator", xs => { val a = DoubleAccumulator.fromSpecific(xs.map(_.toDouble)); (xs.map(_.toDouble).toList, () => a.stepper) }), ("AnyAccumulator", xs => { val a = AnyAccumulator.from(xs.map("a" + _)); (xs.map("a" + _).toList, () => a.stepper) }),
    ("immutable.Queue", xs => { val q = im.Queue.from(xs.take(xs.size / 2)).enqueueAll(xs.drop(xs.size / 2)); (xs.toList, () => q.stepper) }), ("mutable.Queue", xs => (xs.toList, () => mu.Queue.from(xs).stepper)),
    ("immutable.IntMap", xs => { val m = im.IntMap.from(xs.map(x => x -> x)); (m.toList, () => m.stepper) }), ("immutable.ListSet", xs => { val s = im.ListSet.from(xs.take(200)); (s.toList, () => s.stepper) }),
    ("Vector.big", xs => { val v = Vector.tabulate(xs.size * 37)(i => i); (v.toList, () => v.stepper) }), ("Vector.sliced", xs => { val v = Vector.tabulate(xs.size * 37 + 40)(i => i).slice(17, xs.size * 30); (v.toList, () => v.stepper) })
  )
}

class StepperFam extends Fam {
  type P = SCase
  val name = "Stepper"
  def gen(r: Random) = { val n = r.nextInt(8) match { case 0 => 0; case 1 => 1 + r.nextInt(3); case 2 | 3 | 4 => r.nextInt(64); case 5 | 6 => r.nextInt(1100); case _ => r.nextInt(5000) }
    Vector(SCase(r.nextInt(StepperFam.kinds.size), Vector.fill(n)(r.nextInt(if (r.nextBoolean()) 50 else 100000)), r.nextLong())) }
  def run(prog: Vector[SCase]): Unit = prog.foreach { c =>
    val (k, f) = StepperFam.kinds(c.kind); val (exp, mk) = f(c.xs); val r = new Random(c.seed); val nm = s"Stepper[$k]"
    def norm(x: Any): Any = x match { case c: Char => c.toInt; case b: Byte => b.toInt; case s: Short => s.toInt; case f: Float => f.toDouble; case x => x }
    // plain drain
    chk(nm, "drain")(exp.sortBy(_.toString))({ val s = mk(); val b = mu.ListBuffer.empty[Any]; while (s.hasStep) b += norm(s.nextStep()); b.toList.sortBy(_.toString) })
    val s0 = mk(); val ch = s0.characteristics
    if ((ch & ORDERED) != 0) chk(nm, "drainOrdered")(exp)({ val b = mu.ListBuffer.empty[Any]; while (s0.hasStep) b += norm(s0.nextStep()); b.toList })
    def explore(s: Stepper[_], depth: Int): List[Any] = {
      val c0 = s.characteristics; val est = s.estimateSize
      val pre = mu.ListBuffer.empty[Any]
      if (r.nextInt(3) == 0) { var j = r.nextInt(4); while (j > 0 && s.hasStep) { pre += norm(s.nextStep()); j -= 1 } }
      val rest: List[Any] = if (depth < 7 && r.nextInt(4) != 0) {
        val p = s.trySplit()
        if (p == null) { val b = mu.ListBuffer.empty[Any]; while (s.hasStep) b += norm(s.nextStep()); b.toList }
        else {
          if ((c0 & SUBSIZED) != 0) check(nm, "SUBSIZED", (p.characteristics & SIZED) != 0 && (s.characteristics & SIZED) != 0, s"after split ${p.characteristics} ${s.characteristics}")
          val pe = p.estimateSize; val se = s.estimateSize
          val a = explore(p, depth + 1); val b = explore(s, depth + 1)
          if ((c0 & SUBSIZED) != 0) check(nm, "SUBSIZED/exact", pe == a.size && se == b.size, s"split estimates $pe,$se actual ${a.size},${b.size}")
          a ++ b
        }
      } else { val b = mu.ListBuffer.empty[Any]; while (s.hasStep) b += norm(s.nextStep()); b.toList }
      val all = pre.toList ++ rest
      if ((c0 & SIZED) != 0) check(nm, "SIZED", est == all.size, s"estimateSize $est but ${all.size} elements")
      all
    }
    val s1 = mk(); val c1 = s1.characteristics; val got = explore(s1, 0)
    chk(nm, "splitAll")(exp.sortBy(_.toString))(got.sortBy(_.toString))
    if ((c1 & ORDERED) != 0) chk(nm, "splitOrdered")(exp)(got)
    if ((c1 & DISTINCT) != 0) check(nm, "DISTINCT", got.distinct.size == got.size, "duplicates")
    if ((c1 & SORTED) != 0) check(nm, "SORTED", got == got.sortBy(_.toString.toInt), "not sorted")
    if ((c1 & NONNULL) != 0) check(nm, "NONNULL", !got.contains(null), "null")
    // the Java spliterator view
    chk(nm, "spliterator")((exp, exp.size.toLong))({ val sp = mk().spliterator[Any]; val b = mu.ListBuffer.empty[Any]; val sz = sp.getExactSizeIfKnown
      if (r.nextBoolean()) { sp.tryAdvance(x => b += norm(x)) }; sp.forEachRemaining(x => b += norm(x)); (if ((c1 & ORDERED) != 0) b.toList else exp, if (sz >= 0) sz else exp.size.toLong) })
    chk(nm, "javaIterator")(exp)({ val it = mk().javaIterator[Any]; val b = mu.ListBuffer.empty[Any]; while (it.hasNext) b += norm(it.next()); if ((c1 & ORDERED) != 0) b.toList else exp })
  }
}

final case class StrCase(kind: Int, xs: Vector[Int]) { override def toString = s"kind=$kind n=${xs.size} xs=${xs.take(20).mkString(",")}" }

class StreamFam extends Fam {
  type P = StrCase
  val name = "Streams"
  def gen(r: Random) = { val n = r.nextInt(6) match { case 0 => 0; case 1 | 2 => r.nextInt(40); case _ => r.nextInt(5000) }; Vector(StrCase(r.nextInt(10), Vector.fill(n)(r.nextInt(1000) - 300))) }
  def run(prog: Vector[StrCase]): Unit = prog.foreach { c =>
    val xs = c.xs; val l = xs.toList; val sum = l.map(_.toLong).sum
    def ints(nm: String, seq: => java.util.stream.IntStream, par: => java.util.stream.IntStream, exp: List[Int], ordered: Boolean) = {
      chk(nm, "seq.toArray")(if (ordered) exp else exp.sorted)({ val a = seq.toArray.toList; if (ordered) a else a.sorted })
      chk(nm, "par.sum/count/sorted")((exp.map(_.toLong).sum, exp.size.toLong, exp.sorted, exp.distinct.sorted))((par.asLongStream.sum, par.count, par.sorted.toArray.toList, par.distinct.sorted.toArray.toList))
      if (ordered) chk(nm, "par.toArray ordered")(exp)(par.toArray.toList)
      chk(nm, "toScala")((exp.sorted, exp.sorted))({ val a = par.boxed.toScala(List); val b = seq.toScala(Accumulator); (a.map(_.intValue).sorted, b.toList.sorted) })
      chk(nm, "par.filter.map")(exp.filter(_ % 3 == 0).map(_ * 2).sorted)(par.filter(_ % 3 == 0).map(_ * 2).toScala(Vector).sorted)
      chk(nm, "skip/limit")(if (ordered) exp.slice(3, 40) else exp.slice(3, 40).size)({ val a = par.skip(3).limit(37).toArray.toList; if (ordered) a else a.size })
    }
    c.kind match {
      case 0 => ints("List", l.asJavaSeqStream, l.asJavaSeqStream.parallel, l, true)
      case 1 => ints("Vector", xs.asJavaSeqStream, xs.asJavaParStream, l, true)
      case 2 => val a = xs.toArray; ints("Array", a.asJavaSeqStream, a.asJavaParStream, l, true); ints("ArraySeq", im.ArraySeq.unsafeWrapArray(a).asJavaSeqStream, im.ArraySeq.unsafeWrapArray(a).asJavaParStream, l, true)
      case 3 => val s = im.HashSet.from(xs); ints("immutable.HashSet", s.asJavaSeqStream, s.asJavaParStream, s.toList, false)
      case 4 => val b = mu.ArrayBuffer.from(xs); ints("ArrayBuffer", b.asJavaSeqStream, b.asJavaParStream, l, true)
      case 5 => val r = 0 until xs.size * 3 by 3; ints("Range", r.asJavaSeqStream, r.asJavaParStream, r.toList, true)
      case 6 => val m = im.HashMap.from(xs.map(x => x -> (x * 3))); ints("HashMap.keys", m.asJavaSeqKeyStream, m.asJavaParKeyStream, m.keys.toList, false)
        ints("HashMap.values", m.asJavaSeqValueStream, m.asJavaParValueStream, m.values.toList, false)
        chk("HashMap", "pairs")(m.toList.sorted)(m.asJavaParStream.toScala(List).sorted)
      case 7 => val m = im.TreeMap.from(xs.map(x => x -> -x)); ints("TreeMap.keys", m.asJavaSeqKeyStream, m.asJavaParKeyStream, m.keys.toList, true)
        ints("TreeMap.values", m.asJavaSeqValueStream, m.asJavaParValueStream, m.values.toList, true)
      case 8 => val s = xs.map(x => ('a' + (x.abs % 26)).toChar).mkString; ints("String.chars", s.asJavaSeqCharStream, s.asJavaParCharStream, s.toList.map(_.toInt), true)
        ints("String.codePoints", s.asJavaSeqCodePointStream, s.asJavaParCodePointStream, s.toList.map(_.toInt), true)
      case _ => val ss = xs.map("s" + _).toList
        chk("Stream[String]", "seq/par")((ss, ss.sorted, ss.size.toLong))((ss.asJavaSeqStream.toScala(List), ss.toVector.asJavaParStream.sorted.toScala(List), mu.ArrayBuffer.from(ss).asJavaParStream.count))
        chk("Stream[String]", "toScala(Accumulator)")(ss)(ss.asJavaSeqStream.toScala(Accumulator).toList)
        val jl = new java.util.ArrayList[String](ss.asJava)
        chk("Stream[String]", "java stream toScala")((ss, ss.sorted))((jl.stream.toScala(List), jl.parallelStream.toScala(Vector).sorted))
        chk("Stream[Long]", "LongStream")((xs.map(_.toLong).toList, sum))((xs.map(_.toLong).toArray.asJavaSeqStream.toScala(List), xs.map(_.toLong).asJavaParStream.sum))
        chk("Stream[Double]", "DoubleStream")(xs.map(_.toDouble).toList.sorted)(xs.map(_.toDouble).asJavaParStream.toScala(Accumulator).toList.sorted)
    }
  }
}

/** Accumulator programs against an ArrayBuffer of the same elements. */
class AccFam[A, C <: mu.Seq[A] with mu.Builder[A, C]](val name: String, mk: () => C, f: Int => A, drain: (C, C) => Unit) extends Fam {
  type P = Op
  val names = Vector("addOne" -> 8, "addAll" -> 6, "bigAddAll" -> 1, "apply" -> 3, "update" -> 3, "drain" -> 3, "clear" -> 1, "toList" -> 2, "toArray" -> 2, "iterator" -> 2,
    "stepper" -> 2, "map" -> 1, "filter" -> 2, "flatMap" -> 1, "collect" -> 1, "query" -> 2, "to" -> 1, "split" -> 2, "eq" -> 1, "fromSame" -> 1)
  def gen(r: Random) = QGen.ops(r, names, 1 + r.nextInt(25))
  def run(prog: Vector[Op]): Unit = {
    val acc = mk(); val m = mu.ArrayBuffer.empty[A]
    for (op <- prog) {
      val e = f(op.a.toInt); val xs = op.xs.map(x => f(x._1.toInt)); val i = (op.b % (m.size + 2)).toInt - 1; val n = op.n
      n match {
        case "addOne" => acc.addOne(e); m += e
        case "addAll" => acc.addAll(xs); m ++= xs
        case "bigAddAll" => val big = (0 until 4000 + op.b.toInt * 37).map(f); acc ++= big; m ++= big
        case "apply" | "update" if i >= m.size => Excl.hit("WL2 Accumulator apply/update past the size")
        case "apply" => chk(name, n)(m(i))(acc(i))
        case "update" => chk(name, n)(m(i) = e)(acc(i) = e)
        case "drain" => val o = mk(); o ++= xs; if (op.b % 3 == 0) o ++= (0 until 3000).map(f); val oc = o.toList; drain(acc, o); m ++= oc; chk(name, n + "/drained")(Nil)(o.toList)
        case "clear" => acc.clear(); m.clear()
        case "toList" => chk(name, n)(m.toList)(acc.toList)
        case "toArray" => chk(name, n)(m.toList)(acc.toArray[Any].toList)
        case "iterator" => chk(name, n)((m.toList, m.reverseIterator.toList))((acc.iterator.toList, acc.reverseIterator.toList))
        case "stepper" => chk(name, n)(m.toList)({ val s = acc.stepper; val b = mu.ListBuffer.empty[Any]; while (s.hasStep) b += s.nextStep(); b.toList })
        case "map" => chk(name, n)(m.map(_.toString).toList)(acc.map(_.toString).toList)
        case "filter" => chk(name, n)((m.filter(_.hashCode % 3 == 0).toList, m.filterNot(_.hashCode % 3 == 0).toList))((acc.filter(_.hashCode % 3 == 0).toList, acc.filterNot(_.hashCode % 3 == 0).toList))
        case "flatMap" => chk(name, n)(m.flatMap(x => List(x, x)).toList)(acc.flatMap(x => List(x, x)).toList)
        case "collect" => chk(name, n)(m.collect { case x if x.hashCode % 2 == 0 => x }.toList)(acc.collect { case x if x.hashCode % 2 == 0 => x }.toList)
        case "query" => chk(name, n)((m.length, m.knownSize, m.exists(_ == e), m.forall(_ != e), m.count(_ == e), m.indexOf(e), m.headOption, m.lastOption, m.isEmpty))(
          (acc.length, acc.knownSize, acc.exists(_ == e), acc.forall(_ != e), acc.count(_ == e), acc.indexOf(e), acc.headOption, acc.lastOption, acc.isEmpty))
        case "to" => chk(name, n)((m.toVector, m.toList))((acc.to(Vector), acc.to(List)))
        case "split" => chk(name, n)(m.toList)({ val s = acc.stepper; val p = s.trySplit(); val b = mu.ListBuffer.empty[Any]
          if (p != null) while (p.hasStep) b += p.nextStep(); while (s.hasStep) b += s.nextStep(); b.toList })
        case "eq" => chk(name, n)((true, m.hashCode))((acc == m, acc.hashCode))
        case "fromSame" => chk(name, n)(m.toList)(acc.iterableFactory.from(acc).toList)
      }
      chk(name, n + "/content")(m.toList)(acc.toList); chk(name, n + "/size")(m.size)(acc.size)
    }
  }
}

object StreamFams {
  def all: Seq[Fam] = Seq(new StepperFam, new StreamFam,
    new AccFam[Int, IntAccumulator]("IntAccumulator", () => new IntAccumulator, identity, _ drain _),
    new AccFam[Long, LongAccumulator]("LongAccumulator", () => new LongAccumulator, _.toLong * 1000003L, _ drain _),
    new AccFam[Double, DoubleAccumulator]("DoubleAccumulator", () => new DoubleAccumulator, _ / 4.0, _ drain _),
    new AccFam[String, AnyAccumulator[String]]("AnyAccumulator", () => new AnyAccumulator[String], "a" + _, _ drain _))
}
