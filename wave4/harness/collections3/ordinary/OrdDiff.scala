// Ordinary usage at varied sizes, chosen by coverage: ArrayOps / StringOps / ArraySeq / LazyZip (family seq),
// StringParsers (parse), Vector and its builders (vector), immutable HashMap / HashSet bulk operations (hash).
// usage: OrdDiff <seq|parse|vector|hash|all> <seed> <seconds>
import scala.collection.{immutable => im, mutable => mu}
import scala.reflect.ClassTag
import scala.util.Random

final class HK(val i: Int) { // a third of the keys share 50 hash codes
  override def hashCode = if (i % 3 == 0) (i / 3) % 50 else i * 0x9E3779B9
  override def equals(o: Any) = o match { case k: HK => k.i == i; case _ => false }
  override def toString = s"K$i"
}

object OrdDiff {
  var rnd = new Random(1)
  var checks = 0L
  val sigs = mu.LinkedHashMap.empty[String, (Int, String)]
  @volatile var beat = 0L; @volatile var where = ""

  def norm(a: Any): Any = a match {
    case d: Double => "d" + java.lang.Double.doubleToLongBits(d)
    case f: Float => "d" + java.lang.Double.doubleToLongBits(f.toDouble) // a Float stream is a DoubleStream
    case s: String => s.toList
    case arr: Array[_] => arr.iterator.map(norm).toList
    case it: Iterator[_] => it.map(norm).toList
    case s: scala.collection.Set[_] => s.iterator.map(norm).toSet
    case m: scala.collection.Map[_, _] => m.iterator.map { case (k, v) => (norm(k), norm(v)) }.toMap
    case it: Iterable[_] => it.iterator.map(norm).toList
    case p: Product if p.productArity > 0 => p.productPrefix.takeWhile(_ != '$').replaceAll("Tuple\\d+", "T") :: p.productIterator.map(norm).toList
    case x => x
  }
  def ev(f: => Any): Any = try norm(f) catch {
    case _: IndexOutOfBoundsException => "EX:IndexOutOfBounds"
    case e: Exception => "EX:" + e.getClass.getSimpleName
  }
  def chk(fam: String, op: String, detail: => String)(exp: => Any)(act: => Any): Unit = {
    where = s"$fam $op"; beat = System.nanoTime(); checks += 1
    val e = ev(exp); val a = ev(act)
    if (e != a) {
      def kind(x: Any) = x match { case s: String if s.startsWith("EX:") => s; case _ => "val" }
      val sig = s"$fam | $op | ${kind(e)} -> ${kind(a)}"
      val (n, ex) = sigs.getOrElse(sig, (0, ""))
      sigs(sig) = (n + 1, if (n == 0) s"$detail expected ${String.valueOf(e).take(300)} got ${String.valueOf(a).take(300)}" else ex)
    }
  }
  def size(): Int = rnd.nextInt(10) match { case 0 => 0; case 1 => 1; case 2 | 3 => 2 + rnd.nextInt(6); case 4 | 5 | 6 => rnd.nextInt(40); case 7 | 8 => rnd.nextInt(300); case _ => rnd.nextInt(3000) }

  // ---- seq: arrays of every element type, strings, ArraySeq
  def seqCase(): Unit = {
    val n = size(); val m = if (rnd.nextBoolean()) rnd.nextInt(5) else size().min(200)
    def ix() = rnd.nextInt(5) match { case 0 => -1; case 1 => n + 1; case _ => rnd.nextInt(n + 1) }
    val i = ix(); val j = ix(); val k = 1 + rnd.nextInt(6); val v = 3 + rnd.nextInt(50)
    def go[T: ClassTag: Ordering](name: String, g: Int => T, f: T => T): Unit = {
      val a = Array.fill(n)(g(rnd.nextInt(v))); val o = if (rnd.nextInt(4) == 0) a.slice(i, j) else Array.fill(m)(g(rnd.nextInt(v))); val x = g(rnd.nextInt(v + 2))
      val l = a.toList; val lo = o.toList
      rnd.nextInt(3) match {
        case 0 => SeqLikeOps.arrayOps(a, o, l, lo, x, f, i, j, k, s"Array[$name]")
        case 1 => SeqLikeOps.seqOps[T](im.ArraySeq.unsafeWrapArray(a), im.ArraySeq.unsafeWrapArray(o), l, lo, x, f, i, j, k, s"immutable.ArraySeq[$name]")
        case _ => SeqLikeOps.seqOps[T](mu.ArraySeq.make(a), mu.ArraySeq.make(o), l, lo, x, f, i, j, k, s"mutable.ArraySeq[$name]")
      }
      chk(s"ArraySeq[$name]", "equals/hashCode", s"n=$n")((true, true, l.hashCode))((im.ArraySeq.unsafeWrapArray(a) == l, mu.ArraySeq.make(a) == im.ArraySeq.unsafeWrapArray(a.clone), im.ArraySeq.unsafeWrapArray(a).hashCode))
      chk(s"ArrayBuilder[$name]", "addOne/addAll", s"n=$n m=$m")(l ++ lo ++ l.take(3) ++ lo)({ val b = mu.ArrayBuilder.make[T]; if (rnd.nextBoolean()) b.sizeHint(n); a.foreach(b += _); b ++= o; b ++= l.take(3); b.addAll(o, 0, o.length); b.result() })
      chk(s"Array[$name]", "sorting", s"n=$n")(l.sorted)({ val c = a.clone; scala.util.Sorting.stableSort(c); c })
      chk(s"Array[$name]", "stream", s"n=$n")((l, l.reverse))({ import scala.jdk.StreamConverters._; (a.toSeq.asJavaSeqStream.toScala(List), im.ArraySeq.unsafeWrapArray(a).reverseIterator.asJavaSeqStream.toScala(Vector)) })
      chk(s"Array[$name]", "sortInPlace", s"n=$n")(l.sorted)({ val c = a.clone; c.sortInPlace(); c })
    }
    rnd.nextInt(12) match {
      case 0 => go[Int]("Int", x => x, _ + 1)
        val a = Array.fill(n)(rnd.nextInt(v)); val l = a.toList; import scala.jdk.StreamConverters._; import scala.collection.StepperShape
        chk("Stepper[Int]", "array/ArraySeq/Vector/List steppers", s"n=$n")((l, l, l, l, l.map(_.toLong).sum, l.map(_.toDouble), l.take(n / 2).size <= n))((a.stepper.iterator.toList, im.ArraySeq.unsafeWrapArray(a).stepper.iterator.toList, a.toVector.stepper.iterator.toList, l.stepper.iterator.toList, a.map(_.toLong).stepper.iterator.sum, a.map(_.toDouble).stepper.iterator.toList, { val st = a.toVector.stepper; val h = st.trySplit(); h == null || h.iterator.size <= n }))
        chk("Stream[Int]", "asJavaSeqStream/asJavaParStream/toScala", s"n=$n")((l.map(_.toLong).sum, l.filter(_ % 2 == 0), l.sorted, l.map(_ + 1)))((a.toVector.asJavaSeqStream.asLongStream.sum, a.asJavaSeqStream.filter(_ % 2 == 0).toScala(List), a.toVector.asJavaParStream.sorted.toScala(Vector), im.ArraySeq.unsafeWrapArray(a).asJavaParStream.map(_ + 1).toScala(im.ArraySeq)))
      case 1 => go[Long]("Long", _.toLong, _ + 1); case 2 => go[Byte]("Byte", _.toByte, x => (x + 1).toByte)
      case 3 => go[Short]("Short", _.toShort, x => (x + 1).toShort); case 4 => go[Char]("Char", x => ('a' + x).toChar, x => (x + 1).toChar)
      case 5 => go[Double]("Double", _.toDouble, _ + 1); case 6 => go[Float]("Float", _.toFloat, _ + 1); case 7 => go[Boolean]("Boolean", _ % 2 == 0, !_)
      case 8 => go[String]("String", x => "s" + x, _ + "x"); case 9 => go[Unit]("Unit", _ => (), identity)
      case _ =>
        def str(len: Int) = { val sb = new java.lang.StringBuilder; (0 until len).foreach(_ => sb.append(rnd.nextInt(12) match { case 0 => ' '; case 1 => '\n'; case 2 => 'é'; case 3 => '|'; case c => ('a' + rnd.nextInt(v.min(26))).toChar })); sb.toString }
        val s = str(n); val so = if (rnd.nextInt(4) == 0) s.slice(i, j) else str(m); val x = ('a' + rnd.nextInt(v.min(26))).toChar
        SeqLikeOps.strOps(s, so, s.toList, so.toList, x, c => (c + 1).toChar, i, j, k, "String")
        val d = s"n=$n x=$x"
        chk("String", "*", d)(Iterator.fill(k)(s).mkString)(s * k); chk("String", "capitalize", d)(if (s.isEmpty) s else s"${s.head.toUpper}${s.tail}")(s.capitalize)
        chk("String", "stripPrefix", d)(if (s.startsWith(so)) s.substring(so.length) else s)(s.stripPrefix(so)); chk("String", "stripSuffix", d)(if (s.endsWith(so)) s.substring(0, s.length - so.length) else s)(s.stripSuffix(so))
        chk("String", "split(Char)", d)(s.split(java.util.regex.Pattern.quote(x.toString)))(s.split(x)); chk("String", "split(Array[Char])", d)(s.split("[" + x + "|]"))(s.split(Array(x, '|')))
        chk("String", "linesIterator", d)(s.lines().toArray)(s.linesIterator); chk("String", "linesWithSeparators", d)(s)(s.linesWithSeparators.mkString)
        chk("String", "stripMargin", d)(s.split("\n", -1).map(ln => { val t = ln.dropWhile(c => c <= ' '); if (t.startsWith("|")) t.drop(1) else ln }).mkString("\n"))(s.stripMargin)
        chk("String", "StringBuilder", d)((s + so + x + s.reverse, s.length + so.length + 1 + s.length))({ val b = new mu.StringBuilder; b ++= s; b.append(so); b += x; b.appendAll(s.reverseIterator); (b.result(), b.length) })
        chk("String", "StringBuilder.program", d)(true)({
          val sb = new mu.StringBuilder(s); val jb = new java.lang.StringBuilder(s); var ok = true; var st = 0
          while (ok && st < 12) { st += 1; val len = jb.length; val at = rnd.nextInt(len + 1); val to = at + rnd.nextInt(len - at + 1)
            rnd.nextInt(22) match {
              case 0 => sb.append(so); jb.append(so); case 1 => sb.append(x); jb.append(x); case 2 => sb.append(k); jb.append(k); case 3 => sb.append(1.5); jb.append(1.5); case 4 => sb.append(true); jb.append(true)
              case 5 => sb.insert(at, so); jb.insert(at, so); case 6 => sb.insert(at, x); jb.insert(at, x); case 7 => sb.insert(at, 42L); jb.insert(at, 42L); case 8 => sb.delete(at, to); jb.delete(at, to)
              case 9 => if (len > 0) { sb.deleteCharAt(at.min(len - 1)); jb.deleteCharAt(at.min(len - 1)) }; case 10 => sb.replace(at, to, so); jb.replace(at, to, so); case 11 => sb.reverseInPlace(); jb.reverse()
              case 12 => sb.setLength(at); jb.setLength(at); case 13 => if (len > 0) { sb(at.min(len - 1)) = x; jb.setCharAt(at.min(len - 1), x) }; case 14 => sb.appendAll(so.toCharArray, 0, so.length); jb.append(so.toCharArray, 0, so.length)
              case 15 => sb ++= so; jb.append(so); case 16 => sb.insertAll(at, so.toList); jb.insert(at, so); case 17 => sb.appendAll(so.iterator); jb.append(so); case 18 => sb.insertAll(at, so.toCharArray); jb.insert(at, so.toCharArray)
              case 19 => ok &= sb.indexOf(so) == jb.indexOf(so) && sb.lastIndexOf(so) == jb.lastIndexOf(so) && sb.indexOf(so, at) == jb.indexOf(so, at) && sb.substring(at, to) == jb.substring(at, to) && sb.subSequence(at, to).toString == jb.subSequence(at, to).toString
              case 20 => sb.ensureCapacity(len + k); ok &= sb.capacity >= len + k && sb.toCharArray.sameElements(jb.toString.toCharArray) && sb.reverse.toString == new java.lang.StringBuilder(jb).reverse.toString && (len == 0 || sb.charAt(at.min(len - 1)) == jb.charAt(at.min(len - 1)))
              case _ => sb.append(x).append(k).append(so: Any); jb.append(x).append(k).append(so) }
            ok &= sb.toString == jb.toString && sb.length == jb.length && sb.result() == jb.toString && sb.mkString == jb.toString }
          ok })
        chk("String", "steppers", d)((s.chars().toArray.toList, s.codePoints().toArray.toList))((s.charStepper.iterator.toList, s.codePointStepper.iterator.toList))
        chk("String", "format/compare/r", d)((String.format("%s|%5d|%-4s|%c", s, Int.box(k), so, Char.box(x)), Integer.signum(s.compareTo(so)), s.split("\\s+").filter(_.nonEmpty).toList))(("%s|%5d|%-4s|%c".format(s, k, so, x), Integer.signum(s.compare(so)), "\\S+".r.findAllIn(s).toList))
        chk("String", "stripLineEnd", d)(if (s.endsWith("\r\n")) s.dropRight(2) else if (s.endsWith("\n")) s.dropRight(1) else s)(s.stripLineEnd)
        chk("String", "flatMap(String)/concat", d)((s.toList.flatMap(c => List(c, x)).mkString, s + so, so + s, s.toList.map(_.toInt)))((s.flatMap(c => s"$c$x"), s.concat(so.toList), s.prependedAll(so.toList), s.map(_.toInt)))
        chk("String", "StringBuilder.edit", d)(s.patch(i.max(0).min(n), so, 0).reverse)({ val b = new mu.StringBuilder(s); b.insert(i.max(0).min(n), so); b.reverseInPlace().toString })
    }
  }

  // ---- parse: StringParsers against java.lang
  def parseCase(): Unit = {
    def near(b: Long) = (b + rnd.nextInt(5) - 2).toString
    var s = rnd.nextInt(24) match {
      case 0 => near(Int.MaxValue); case 1 => near(Int.MinValue); case 2 => BigInt(Long.MaxValue).+(rnd.nextInt(5) - 2).toString; case 3 => BigInt(Long.MinValue).+(rnd.nextInt(5) - 2).toString
      case 4 => near(Short.MaxValue); case 5 => near(Short.MinValue); case 6 => near(Byte.MaxValue); case 7 => near(Byte.MinValue)
      case 8 => rnd.nextInt().toString; case 9 => rnd.nextLong().toString; case 10 => (rnd.nextInt(2001) - 1000).toString
      case 11 => rnd.nextDouble().toString; case 12 => (rnd.nextGaussian() * math.pow(10, rnd.nextInt(600) - 300)).toString; case 13 => java.lang.Double.toHexString(rnd.nextGaussian() * 1e10)
      case 14 => java.lang.Float.toString(rnd.nextFloat() * 1e10f); case 15 => s"${rnd.nextInt(100)}e${rnd.nextInt(700) - 350}"; case 16 => s".${rnd.nextInt(1000)}"; case 17 => s"${rnd.nextInt(1000)}."
      case 18 => List("NaN", "Infinity", "-Infinity", "+Infinity", "+NaN", "0x1p3", "0x.8p1", "0X1.8P-2", "1d", "1.5f", "1e5D", "0x1p3f", "-0", "+0", "-0.0", "007", "+7", "--7", "", " ", "+", "-", ".", "e1", "1e", "0x", "true", "TRUE", "False", "1_000", "١٢٣", "１２", "1L", "0x10")(rnd.nextInt(34))
      case 19 => "0" * rnd.nextInt(30) + rnd.nextInt(1000); case 20 => "1" + "0" * rnd.nextInt(400); case 21 => "0." + "0" * rnd.nextInt(400) + "1"
      case 22 => BigDecimal(rnd.nextLong(), rnd.nextInt(40)).toString; case _ => rnd.nextInt(1 << rnd.nextInt(31)).toString
    }
    if (rnd.nextInt(3) == 0) { // one mutation
      val alpha = "0123456789+-.eE xXpPdDfFnNaA_\t١L"; val c = alpha(rnd.nextInt(alpha.length)); val at = rnd.nextInt(s.length + 1)
      s = rnd.nextInt(3) match { case 0 => s.take(at) + c + s.drop(at); case 1 => s.take(at) + s.drop(at + 1); case _ => s.take(at) + c + s.drop(at + 1) }
    }
    if (rnd.nextInt(10) == 0) s = " " * rnd.nextInt(3) + s + " " * rnd.nextInt(3)
    def j[A](f: String => A): Option[A] = try Some(f(s)) catch { case _: NumberFormatException => None }
    val d = "\"" + s + "\""
    chk("StringParsers", "toIntOption", d)(j(java.lang.Integer.parseInt))(s.toIntOption); chk("StringParsers", "toLongOption", d)(j(java.lang.Long.parseLong))(s.toLongOption)
    chk("StringParsers", "toShortOption", d)(j(java.lang.Short.parseShort))(s.toShortOption); chk("StringParsers", "toByteOption", d)(j(java.lang.Byte.parseByte))(s.toByteOption)
    chk("StringParsers", "toDoubleOption", d)(j(java.lang.Double.parseDouble))(s.toDoubleOption); chk("StringParsers", "toFloatOption", d)(j(java.lang.Float.parseFloat))(s.toFloatOption)
    chk("StringParsers", "toBooleanOption", d)(if (s.equalsIgnoreCase("true")) Some(true) else if (s.equalsIgnoreCase("false")) Some(false) else None)(s.toBooleanOption)
    chk("StringParsers", "toInt", d)(java.lang.Integer.parseInt(s))(s.toInt); chk("StringParsers", "toDouble", d)(java.lang.Double.parseDouble(s))(s.toDouble)
  }

  // ---- vector: programs of ordinary calls against an Array[Int] model
  def cat(a: Array[Int], b: Array[Int]) = { val r = java.util.Arrays.copyOf(a, a.length + b.length); System.arraycopy(b, 0, r, a.length, b.length); r }
  def sub(a: Array[Int], from: Int, until: Int) = { val f = from.max(0).min(a.length); val u = until.max(f).min(a.length); java.util.Arrays.copyOfRange(a, f, u) }
  def vsize(): Int = rnd.nextInt(20) match {
    case 0 => 0; case 1 | 2 | 3 => rnd.nextInt(70); case 4 | 5 | 6 => 1024 - 40 + rnd.nextInt(80); case 7 | 8 => 32768 - 40 + rnd.nextInt(80); case 9 | 10 | 11 => rnd.nextInt(3000)
    case 12 | 13 => rnd.nextInt(70000); case 14 => 1048576 - 40 + rnd.nextInt(80); case 15 => 32 * (1 + rnd.nextInt(40)); case 16 => 1024 * (1 + rnd.nextInt(40)); case _ => rnd.nextInt(400)
  }
  var ctr = 0
  def fresh(n: Int): Array[Int] = { val a = new Array[Int](n); var i = 0; while (i < n) { a(i) = ctr; ctr += 1; i += 1 }; a }
  def source(m: Array[Int]): IterableOnce[Int] = rnd.nextInt(9) match {
    case 0 => m.toList; case 1 => Vector.from(m); case 2 => m.iterator; case 3 => im.ArraySeq.unsafeWrapArray(m); case 4 => m.view; case 5 => mu.ArrayBuffer.from(m)
    case 6 => (Vector.from(fresh(rnd.nextInt(50))) ++ m).drop(0).takeRight(m.length); case 7 => im.LazyList.from(m); case _ => m.toSet.toList.sorted.to(im.TreeSet) match { case s if s.size == m.length && m.sameElements(s) => s; case _ => m.toSeq }
  }
  def build(m: Array[Int]): Vector[Int] = rnd.nextInt(8) match {
    case 0 => Vector.from(m); case 1 => m.toVector; case 2 => m.foldLeft(Vector.empty[Int])(_ :+ _); case 3 => m.foldRight(Vector.empty[Int])(_ +: _)
    case 4 => val b = Vector.newBuilder[Int]; if (rnd.nextBoolean()) b.sizeHint(m.length); m.foreach(b += _); b.result()
    case 5 => // builder fed chunks from different kinds of source
      val b = new im.VectorBuilder[Int]; var at = 0
      while (at < m.length) { val len = (if (rnd.nextBoolean()) 1 + rnd.nextInt(40) else vsize()).min(m.length - at); val ch = sub(m, at, at + len); if (len == 1 && rnd.nextBoolean()) b.addOne(ch(0)) else b.addAll(source(ch)); at += len; if (rnd.nextInt(50) == 0) b.result() }
      b.result()
    case 6 => val h = m.length / 2; Vector.from(sub(m, 0, h)) ++ Vector.from(sub(m, h, m.length))
    case _ => val h = rnd.nextInt(m.length + 1); Vector.from(sub(m, h, m.length)).prependedAll(source(sub(m, 0, h)))
  }
  def same(op: String, d: => String, v: Vector[Int], m: Array[Int]): Unit = {
    chk("Vector", op + " length", d)(m.length)(v.length)
    if (v.length == m.length) {
      if (m.length <= 5000 || rnd.nextInt(8) == 0) { chk("Vector", op + " iterator", d)(true)({ val it = v.iterator; var i = 0; var ok = true; while (it.hasNext && i < m.length) { ok &= it.next() == m(i); i += 1 }; ok && !it.hasNext && i == m.length }); chk("Vector", op + " foreach", d)(true)({ var i = 0; var ok = true; v.foreach { x => ok &= x == m(i); i += 1 }; ok }) }
      else chk("Vector", op + " apply", d)(true)((0 until 300).forall { _ => val i = rnd.nextInt(m.length); v(i) == m(i) } && v.head == m(0) && v.last == m(m.length - 1) && (0 until m.length by 1021).forall(i => v(i) == m(i)))
    }
  }
  def vectorCase(): Unit = {
    var m = fresh(vsize()); var v = build(m); val trace = new StringBuilder(s"build(${m.length})")
    same("build", trace.toString, v, m)
    var steps = 1 + rnd.nextInt(6)
    while (steps > 0) {
      steps -= 1; val n = m.length; val i = rnd.nextInt(n + 1); val j = rnd.nextInt(n + 1); val o = fresh(if (rnd.nextBoolean()) rnd.nextInt(40) else vsize())
      val op = rnd.nextInt(35) match {
        case 0 => v = v :+ -1; m = cat(m, Array(-1)); "appended"
        case 1 => v = -2 +: v; m = cat(Array(-2), m); "prepended"
        case 2 => v = v.appendedAll(source(o)); m = cat(m, o); s"appendedAll(${o.length})"
        case 3 => v = v.prependedAll(source(o)); m = cat(o, m); s"prependedAll(${o.length})"
        case 4 => v = v ++ build(o); m = cat(m, o); s"++ Vector(${o.length})"
        case 5 => v = build(o) ++ v; m = cat(o, m); s"Vector(${o.length}) ++"
        case 6 => v = v.take(i); m = sub(m, 0, i); s"take($i)"
        case 7 => v = v.drop(i); m = sub(m, i, n); s"drop($i)"
        case 8 => v = v.slice(i, j); m = sub(m, i, j); s"slice($i,$j)"
        case 9 => v = v.takeRight(i); m = sub(m, n - i, n); s"takeRight($i)"
        case 10 => v = v.dropRight(i); m = sub(m, 0, n - i); s"dropRight($i)"
        case 11 => if (n > 0) { v = v.tail; m = sub(m, 1, n) }; "tail"
        case 12 => if (n > 0) { v = v.init; m = sub(m, 0, n - 1) }; "init"
        case 13 => if (n > 0) { val at = i.min(n - 1); v = v.updated(at, -3); m = m.clone; m(at) = -3 }; s"updated($i)"
        case 14 => v = v.map(_ + 1); m = m.clone; var q = 0; while (q < n) { m(q) += 1; q += 1 }; "map"
        case 15 => v = v.filter(_ % 3 != 0); m = { val b = new java.util.ArrayList[Int]; m.foreach(x => if (x % 3 != 0) b.add(x)); val r = new Array[Int](b.size); var q = 0; while (q < r.length) { r(q) = b.get(q); q += 1 }; r }; "filter"
        case 16 => v = v.reverse; m = { val r = new Array[Int](n); var q = 0; while (q < n) { r(q) = m(n - 1 - q); q += 1 }; r }; "reverse"
        case 17 => val r = rnd.nextInt(5); v = v.patch(i, source(o), r); m = cat(cat(sub(m, 0, i), o), sub(m, i + r, n)); s"patch($i,${o.length},$r)"
        case 18 => val (a, b) = v.splitAt(i); v = b ++ a; m = cat(sub(m, i, n), sub(m, 0, i)); s"splitAt($i) swapped"
        case 19 => val g = 1 + rnd.nextInt(2000); if (n < 200000) v = v.grouped(g).foldLeft(Vector.empty[Int])(_ ++ _); s"grouped($g).flatten"
        case 20 => v = v.iterator.slice(i, j).toVector; m = sub(m, i, j); s"iterator.slice($i,$j).toVector"
        case 21 => val a = new Array[Int](n + 2); val c = v.copyToArray(a, 1); chk("Vector", "copyToArray", trace.toString)((n, true))((c, java.util.Arrays.equals(sub(a, 1, n + 1), m))); "copyToArray"
        case 22 => v = v.padTo(n + o.length, -4); m = cat(m, Array.fill(o.length)(-4)); s"padTo(+${o.length})"
        case 23 => val b = Vector.newBuilder[Int]; b ++= v; b ++= source(o); if (rnd.nextBoolean()) { b += -5; m = cat(cat(m, o), Array(-5)) } else m = cat(m, o); v = b.result(); s"builder ++= self ++= ${o.length}"
        case 24 => val it = v.iterator; val pre = i.min(n).min(100); var q = 0; while (q < pre) { it.next(); q += 1 }; val ks = it.knownSize; val w = it.drop((j / 2).min(n)).toVector; chk("Vector", "iterator.knownSize", trace.toString)(n - pre)(ks); v = w; m = sub(m, pre + (j / 2).min(n), n); s"iterator.next*$pre.drop(${j / 2}).toVector"
        case 26 => var q = 0; val c = rnd.nextInt(80).min(n); while (q < c) { v = if (q % 2 == 0) v.tail else v.init; q += 1 }; m = sub(m, (c + 1) / 2, n - c / 2); s"tail/init*$c"
        case 27 => v = v.appendedAll(v); m = cat(m, m); if (m.length > 3000000) { v = v.take(1000); m = sub(m, 0, 1000) }; "appendedAll(self)"
        case 28 => val w = build(o); v = v.take(i) ++ w ++ v.drop(i); m = cat(cat(sub(m, 0, i), o), sub(m, i, n)); s"take($i) ++ Vector(${o.length}) ++ drop($i)"
        case 29 => val it = v.iterator; val a = new Array[Int](n + 1); val c1 = it.copyToArray(a, 0, i); val rest = it.slice(0, n).toVector; chk("Vector", "iterator.copyToArray", trace.toString)((i, true))((c1, java.util.Arrays.equals(sub(a, 0, i), sub(m, 0, i)))); v = rest; m = sub(m, i, n); s"iterator.copyToArray($i).rest"
        case 30 => v = v.view.slice(i, j).toVector; m = sub(m, i, j); s"view.slice($i,$j).toVector"
        case 31 => chk("Vector", "stepper", trace.toString)(m.foldLeft(0L)(_ + _))({ val st = v.stepper; var z = 0L; while (st.hasStep) z += st.nextStep(); z }); "stepper"
        case 32 => v = v.sorted; m = m.clone; java.util.Arrays.sort(m); "sorted"
        case 33 => v = v.flatMap(x => if (x % 5 == 0) Vector(x, x) else if (x % 5 == 1) Nil else List(x)); m = { val b = new java.util.ArrayList[Int]; m.foreach(x => if (x % 5 == 0) { b.add(x); b.add(x) } else if (x % 5 != 1) b.add(x)); val r = new Array[Int](b.size); var q = 0; while (q < r.length) { r(q) = b.get(q); q += 1 }; r }; "flatMap"
        case _ => chk("Vector", "reads", trace.toString)((if (n > 0) m(i.min(n - 1)) else -9, m.indexOf(-3), m.foldLeft(0L)(_ + _)))((if (n > 0) v.reverseIterator.drop(n - 1 - i.min(n - 1)).next() else -9, v.indexOf(-3), v.foldLeft(0L)(_ + _))); "reads"
      }
      trace.append(".").append(op)
      same(op.takeWhile(c => c != '(' && c != ' '), trace.toString, v, m)
    }
    chk("Vector", "equals/hashCode", trace.toString)((true, true))({ val w = Vector.from(m); (v == w && w == v, v.hashCode == w.hashCode) })
  }

  // ---- hash: immutable HashMap and HashSet bulk operations against TreeMap / TreeSet keyed by the Int
  def hashCase(): Unit = {
    val n = rnd.nextInt(8) match { case 0 => rnd.nextInt(5); case 1 | 2 => rnd.nextInt(40); case 3 | 4 | 5 => rnd.nextInt(600); case 6 => rnd.nextInt(5000); case _ => rnd.nextInt(30000) }
    val u = (n * (1 + rnd.nextInt(3))).max(4)
    def pairs(c: Int) = List.fill(c)((rnd.nextInt(u), rnd.nextInt(100)))
    val pa = pairs(n); val ma = im.TreeMap.from(pa); val a = im.HashMap.from(pa.map { case (k, v) => (new HK(k), v) })
    val (b, mb): (im.HashMap[HK, Int], im.TreeMap[Int, Int]) = rnd.nextInt(6) match {
      case 0 => (a, ma)
      case 1 | 2 => val ch = pairs(1 + rnd.nextInt(6)); val rm = pairs(rnd.nextInt(4)).map(_._1); ((a ++ ch.map { case (k, v) => (new HK(k), v) }) -- rm.map(new HK(_)), (ma ++ ch) -- rm) // shares structure with a
      case 3 => val t = rnd.nextInt(u); (a.filter(_._1.i < t), ma.filter(_._1 < t))
      case _ => val pb = pairs(if (rnd.nextBoolean()) rnd.nextInt(10) else rnd.nextInt(n + 1)); (im.HashMap.from(pb.map { case (k, v) => (new HK(k), v) }), im.TreeMap.from(pb))
    }
    val d = s"|a|=${ma.size} |b|=${mb.size} universe=$u"
    def nm(r: scala.collection.Map[HK, Int]): Any = { val l = r.iterator.map { case (k, v) => (k.i, v) }.toList.sorted; if (l.size != r.size) "size " + r.size + " but " + l.size + " entries" else if (!l.forall { case (k, v) => r.get(new HK(k)).contains(v) }) "iterated entry not found by get" else l }
    def ns(r: scala.collection.Set[HK]): Any = { val l = r.iterator.map(_.i).toList.sorted; if (l.size != r.size) "size " + r.size + " but " + l.size + " elements" else if (!l.forall(k => r.contains(new HK(k)))) "iterated element not found by contains" else l }
    def c(op: String)(exp: => Any)(act: => Any) = chk("HashMap", op, d)(exp)(act)
    val p = (k: Int) => k % 3 != 1; val keysB = mb.keys.toList
    c("from")(ma.toList)(nm(a)); c("++")((ma ++ mb).toList)(nm(a ++ b)); c("concat(List)")((ma ++ mb).toList)(nm(a.concat(b.toList))); c("concat(mutable)")((ma ++ mb).toList)(nm(a ++ mu.HashMap.from(b)))
    c("merged")(ma.foldLeft(mb) { case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(v)(v + _)) }.toList)(nm(a.merged(b) { case ((k, v), (_, w)) => (k, v + w) }))
    c("merged(null)")((mb ++ ma).toList)(nm(a.merged(b)(null)))
    c("removedAll(keySet)")((ma -- keysB).toList)(nm(a.removedAll(b.keySet))); c("removedAll(List)")((ma -- keysB).toList)(nm(a.removedAll(b.keys.toList))); c("--(iterator)")((ma -- keysB).toList)(nm(a -- b.keysIterator))
    c("removedAll(HashSet)")((ma -- keysB).toList)(nm(a.removedAll(im.HashSet.from(b.keys)))); c("removedAll(mutable.HashSet)")((ma -- keysB).toList)(nm(a.removedAll(mu.HashSet.from(b.keys))))
    c("filter")(ma.filter(e => p(e._1)).toList)(nm(a.filter(e => p(e._1.i)))); c("filterNot")(ma.filterNot(e => p(e._1)).toList)(nm(a.filterNot(e => p(e._1.i))))
    c("partition")({ val (x, y) = ma.partition(e => p(e._2)); (x.toList, y.toList) })({ val (x, y) = a.partition(e => p(e._2)); (nm(x), nm(y)) })
    c("transform")(ma.transform((k, v) => k + v).toList)(nm(a.transform((k, v) => k.i + v))); c("map(collide)")(ma.map { case (k, v) => (k / 2, v) }.keySet.toList)(nm(a.map { case (k, v) => (new HK(k.i / 2), v) }) match { case l: List[(Int, Int) @unchecked] => l.map(_._1); case x => x })
    c("==")(ma == mb)(a == b); c("== rebuilt")((true, true))({ val r = im.HashMap.from(ma.toList.reverse.map { case (k, v) => (new HK(k), v) }); (a == r && r == a, a.hashCode == r.hashCode) })
    c("updatedWith")(ma.updatedWith(u / 2)(o => o.map(_ + 1).filter(_ % 2 == 0).orElse(Some(1).filter(_ => o.isEmpty))).toList)(nm(a.updatedWith(new HK(u / 2))(o => o.map(_ + 1).filter(_ % 2 == 0).orElse(Some(1).filter(_ => o.isEmpty)))))
    c("fold/iterators")((ma.size, ma.keysIterator.map(_.toLong).sum, ma.valuesIterator.map(_.toLong).sum))({ var e = 0L; a.foreachEntry((k, v) => e += v); (a.iterator.size, a.keysIterator.map(_.i.toLong).sum, e.max(a.valuesIterator.map(_.toLong).sum)) })
    c("builder")((ma ++ mb + (-1 -> 1) + (-2 -> 2)).toList)({ val bd = im.HashMap.newBuilder[HK, Int]; bd ++= a; bd ++= b; bd += (new HK(-1) -> 1); val r1 = bd.result(); bd += (new HK(-2) -> 2); val r2 = bd.result(); if (r1.contains(new HK(-2))) "first result changed by a later addOne" else nm(r2) })
    c("builder(pairs)")(ma.toList)({ val bd = im.HashMap.newBuilder[HK, Int]; pa.foreach { case (k, v) => bd.addOne(new HK(k), v) }; nm(bd.result()) })
    c("removed loop")((ma -- keysB.take(50)).toList)(nm(keysB.take(50).foldLeft(a)((m, k) => m.removed(new HK(k))))); c("collect")(ma.collect { case (k, v) if p(k) => (k, v + 1) }.toList)(nm(a.collect { case (k, v) if p(k.i) => (k, v + 1) }))
    c("keySet ops")(((ma.keySet diff mb.keySet).toList, (ma.keySet intersect mb.keySet).toList, (ma.keySet union mb.keySet).toList, ma.keySet.subsetOf(mb.keySet)))((ns(a.keySet diff b.keySet), ns(a.keySet intersect b.keySet), ns(a.keySet union b.keySet), a.keySet.subsetOf(b.keySet)))
    c("groupBy/partitionMap")(ma.groupBy(_._2 % 3).map { case (k, v) => (k, v.toList) })(a.groupBy(_._2 % 3).map { case (k, v) => (k, nm(v)) })
    // sets
    def cs(op: String)(exp: => Any)(act: => Any) = chk("HashSet", op, d)(exp)(act)
    val sa = im.HashSet.from(pa.map(e => new HK(e._1))); val ta = ma.keySet; val tb = mb.keySet
    val sb = rnd.nextInt(3) match { case 0 => im.HashSet.from(keysB.map(new HK(_))); case 1 => (sa -- (ta diff tb).toList.map(new HK(_))) ++ (tb diff ta).toList.map(new HK(_)); case _ => b.keySet.to(im.HashSet) }
    cs("from")(ta.toList)(ns(sa)); cs("b")(tb.toList)(ns(sb)); cs("union")((ta union tb).toList)(ns(sa union sb)); cs("diff")((ta diff tb).toList)(ns(sa diff sb)); cs("intersect")((ta intersect tb).toList)(ns(sa intersect sb))
    cs("concat(List)")((ta union tb).toList)(ns(sa ++ keysB.map(new HK(_)))); cs("removedAll(List)")((ta diff tb).toList)(ns(sa.removedAll(keysB.map(new HK(_))))); cs("removedAll(keySet)")((ta diff tb).toList)(ns(sa -- b.keySet)); cs("diff(mutable)")((ta diff tb).toList)(ns(sa diff mu.HashSet.from(sb)))
    cs("filter")(ta.filter(p).toList)(ns(sa.filter(k => p(k.i)))); cs("filterNot")(ta.filterNot(p).toList)(ns(sa.filterNot(k => p(k.i)))); cs("partition")({ val (x, y) = ta.partition(p); (x.toList, y.toList) })({ val (x, y) = sa.partition(k => p(k.i)); (ns(x), ns(y)) })
    cs("subsetOf")((ta.subsetOf(tb), tb.subsetOf(ta), true))((sa.subsetOf(sb), sb.subsetOf(sa), (sa intersect sb).subsetOf(sa))); cs("==")((ta == tb, true))((sa == sb, { val r = im.HashSet.from(ta.toList.reverse.map(new HK(_))); r == sa && r.hashCode == sa.hashCode }))
    cs("map(collide)")(ta.map(_ / 2).toList)(ns(sa.map(k => new HK(k.i / 2)))); cs("builder")((ta union tb + -1 + -2).toList)({ val bd = im.HashSet.newBuilder[HK]; bd ++= sa; bd ++= sb; bd += new HK(-1); val r1 = bd.result(); bd += new HK(-2); val r2 = bd.result(); if (r1.contains(new HK(-2))) "first result changed by a later addOne" else ns(r2) })
    cs("incl/excl loop")(((ta ++ keysB.take(40)) -- ta.take(40)).toList)(ns(ta.take(40).foldLeft(keysB.take(40).foldLeft(sa)((s, k) => s.incl(new HK(k))))((s, k) => s.excl(new HK(k)))))
  }

  def main(args: Array[String]): Unit = {
    val fam = args(0); rnd = new Random(args(1).toLong); val secs = args(2).toInt
    val fams: List[(String, () => Unit)] = List[(String, () => Unit)]("seq" -> (() => seqCase()), "parse" -> (() => parseCase()), "vector" -> (() => vectorCase()), "hash" -> (() => hashCase())).filter(f => fam == "all" || f._1 == fam)
    val counts = mu.LinkedHashMap.empty[String, Long]
    val worker = new Thread(() => for ((name, run) <- fams) { val deadline = System.nanoTime() + secs * 1000000000L / fams.size; var n = 0L; while (System.nanoTime() < deadline) { run(); n += 1 }; counts(name) = n })
    worker.setDaemon(true); beat = System.nanoTime(); worker.start()
    while (worker.isAlive) { worker.join(500); if (worker.isAlive && System.nanoTime() - beat > 20000000000L) { println(s"HANG (no result after 20s) at: $where"); report(counts); Runtime.getRuntime.exit(2) } }
    report(counts); Runtime.getRuntime.exit(0)
  }
  def report(counts: scala.collection.Map[String, Long]): Unit = {
    println(s"==== cases=${counts.map { case (k, v) => s"$k:$v" }.mkString(" ")} checks=$checks signatures=${sigs.size}")
    sigs.foreach { case (s, (n, ex)) => println(s"$n\t$s\n\t\t$ex") }
  }
}
