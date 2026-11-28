//> using scala 2.13.18
//> using dep org.scala-lang:scala-compiler:2.13.18
//> using dep org.openjdk.jol:jol-core:0.17

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import org.openjdk.jol.info.{ClassData, FieldData}

object Layout {
  val TreeCls = "scala.reflect.internal.Trees$Tree"
  val SymTreeCls = "scala.reflect.internal.Trees$SymTree"

  case class Scenario(name: String, dropId: Boolean, symOnTree: Boolean, dropOuter: Boolean)
  val Base = Scenario("base", false, false, false)
  val scenarios = List(Base, Scenario("A:-id", true, false, false), Scenario("B:+sym", false, true, false), Scenario("A+B", true, true, false),
    Scenario("C:-outer", false, false, true), Scenario("A+B+C", true, true, true))

  def isId(f: FieldData) = f.hostClass == TreeCls.replace('$','.') && f.name == "id"
  def isSym(f: FieldData) = f.hostClass == SymTreeCls.replace('$','.') && f.name == "symbol"

  def fieldSize(t: String) = t match {
    case "long" | "double" => 8; case "int" | "float" => 4; case "short" | "char" => 2; case "boolean" | "byte" => 1; case _ => 4 }
  /** Fields per scenario (JOL's ClassData field enumeration of the whole hierarchy). */
  def fields(cls: Class[_], s: Scenario): List[(String, String, Int)] = {
    val fs = ClassData.parseClass(cls).fields.asScala.toList
    val kept = fs.filterNot(f => (isId(f) && s.dropId) || (isSym(f) && s.symOnTree) || (s.dropOuter && f.name == "$outer" && f.hostClass.endsWith("TreeContextApiImpl")))
    val isTree = classOf[scala.reflect.internal.Trees#Tree].isAssignableFrom(cls)
    val extra = if (isTree && s.symOnTree) List(("Trees.Tree", "symbol", 4)) else Nil
    kept.map(f => (f.hostClass.stripPrefix("scala.reflect.internal."), f.name, fieldSize(f.typeClass))) ++ extra
  }
  /** JDK 15+ field layout: fields packed largest-first, gaps in superclass layout filled; with 4-byte fields
   *  the instance size is align8(header + sum). Validated against the live heap histogram below. */
  def size(cls: Class[_], s: Scenario, header: Int): Int = { val raw = header + fields(cls, s).map(_._3).sum; (raw + 7) & ~7 }
  case class L(header: Int) { def layout(c: Class[_], s: Scenario) = size(c, s, header) }

  def main(args: Array[String]): Unit = {
    val models = List("12B-hdr" -> L(12), "8B-hdr" -> L(8))

    // Print the field layouts of the interesting classes for the record.
    val show = List(TreeCls, SymTreeCls, "scala.reflect.internal.Trees$Ident", "scala.reflect.internal.Trees$Select",
      "scala.reflect.internal.Trees$TypeTree", "scala.reflect.internal.Trees$Apply", "scala.reflect.internal.Trees$Literal",
      "scala.reflect.internal.Trees$DefDef",
      "scala.reflect.internal.util.OffsetPosition", "scala.reflect.internal.util.RangePosition",
      "scala.reflect.internal.util.TransparentPosition", "scala.reflect.macros.SingleAttachment", "scala.reflect.macros.NonemptyAttachments")
    for (n <- show) { val c = Class.forName(n); println(s"$n base=${size(c, Base, 12)}B: " + fields(c, Base).map(f => s"${f._1}.${f._2}:${f._3}").mkString(", ")) }

    for (corpus <- args) {
      val lines = Files.readAllLines(Paths.get(s"out/$corpus.tsv")).asScala.map(_.split('\t')).toList
      def isTree(n: String) = n.startsWith("scala.reflect.internal.Trees$") && !n.contains("$$Lambda") && {
        val c = Class.forName(n); classOf[scala.reflect.internal.Trees#Tree].isAssignableFrom(c)
      }
      val heap = lines.collect { case Array("H", c, n, b) if isTree(c) => (c, n.toLong, b.toLong) }
      val walk = lines.collect { case Array("W", "tree", c, n) => (c, n.toLong) }.toMap
      val coh = Objects.histo(corpus + "-coh")
      val posHeap = lines.collect { case Array("H", c, n, b) if c.startsWith("scala.reflect.internal.util.") && c.endsWith("Position") || c.startsWith("scala.reflect.macros.") && c.endsWith("Attachment") || c == "scala.reflect.macros.NonemptyAttachments" => (c, n.toLong, b.toLong) }
      val allHeapBytes = lines.collect { case Array("H", _, _, b) => b.toLong }.sum

      println(s"\n#### corpus $corpus  (live heap after typer: ${allHeapBytes >> 20} MB)")
      println("class\theapN\twalkN\theapBytes\t" + (for ((m, _) <- models; s <- scenarios) yield s"$m/${s.name}").mkString("\t") + "\tsymTree?")
      val tot = scala.collection.mutable.Map[String, Long]().withDefaultValue(0L)
      val totWalk = scala.collection.mutable.Map[String, Long]().withDefaultValue(0L)
      for ((c, n, b) <- heap.sortBy(-_._3)) {
        val cls = Class.forName(c)
        val sizes = for ((m, l) <- models; s <- scenarios) yield {
          val sz = l.layout(cls, s)
          tot(s"$m/${s.name}") += sz * n
          totWalk(s"$m/${s.name}") += sz * walk.getOrElse(c, 0L)
          sz
        }
        val sym = classOf[scala.reflect.internal.Trees#SymTree].isAssignableFrom(cls)
        val actual = b / n; if (actual != sizes.head) println(s"!! MODEL MISMATCH $c actual=$actual model=${sizes.head}")
        for ((cn, cb) <- coh.get(c); a8 = cb / cn; m8 = sizes(scenarios.size) if a8 != m8) println(s"!! MODEL MISMATCH (8B) $c actual=$a8 model=$m8")
        println(s"${c.stripPrefix("scala.reflect.internal.Trees$")}\t$n\t${walk.getOrElse(c, 0L)}\t$b\t${sizes.mkString("\t")}\t$sym")
      }
      val treeN = heap.map(_._2).sum; val treeB = heap.map(_._3).sum
      println(s"TOTAL heap trees: n=$treeN bytes=$treeB (${"%.1f".format(100.0 * treeB / allHeapBytes)}% of live heap); walk trees n=${walk.values.sum}")
      for ((m, _) <- models) {
        val base = tot(s"$m/base")
        println(s"  $m heap-weighted: " + scenarios.map(s => f"${s.name}=${tot(s"$m/${s.name}")}%d (${100.0 * (tot(s"$m/${s.name}") - base) / base}%+.2f%%)").mkString("  "))
        val wb = totWalk(s"$m/base")
        println(s"  $m walk-weighted: " + scenarios.map(s => f"${s.name}=${totWalk(s"$m/${s.name}")}%d (${100.0 * (totWalk(s"$m/${s.name}") - wb) / wb}%+.2f%%)").mkString("  "))
      }
      println("positions/attachments (heap):")
      for ((c, n, b) <- posHeap.sortBy(-_._3)) println(f"  $c%-50s n=$n%9d bytes=$b%10d  each=${b / n}")
      println(f"  positions total bytes=${posHeap.map(_._3).sum} (${100.0 * posHeap.map(_._3).sum / allHeapBytes}%.1f%% of live heap)")
    }
    for (corpus <- args) Objects.report(corpus)
  }
}

/** Symbol, Type and Name subclasses, and the objects and arrays they own.
 *
 *  Sizes come from a model of HotSpot's field layout (JDK 15+ `FieldLayoutBuilder`), applied class by class from
 *  `Object` down: primitives largest first, then compressed oops (4 bytes); each field goes into the smallest gap
 *  left by the header or a superclass that fits it at its natural alignment, otherwise at the end; the instance is
 *  rounded up to 8. Checked against the histograms taken with 12-byte and with compact (8-byte) headers.
 */
object Objects {
  import java.lang.reflect.Modifier
  case class Fld(host: String, name: String, size: Int, oop: Boolean) { def key = host + "." + name; def label = host.replaceAll(".*[.$]", "") + "." + name.replaceAll(".*\\$\\$", "") }
  /** A candidate change: fields dropped or added per declaring class, and rows (owned objects, arrays) it eliminates. */
  case class Scn(key: String, drop: Set[String] = Set(), add: Map[String, List[Fld]] = Map(), gone: Set[String] = Set()) {
    def ++(o: Scn) = Scn(key + "+" + o.key, drop ++ o.drop, (add.keySet ++ o.add.keySet).map(k => k -> (add.getOrElse(k, Nil) ++ o.add.getOrElse(k, Nil))).toMap, gone ++ o.gone)
  }
  val R = "scala.reflect.internal."
  val Sym = R + "Symbols$Symbol"; val TypeHist = R + "Symbols$TypeHistory"
  val S1 = Scn("S1", drop = Set(Sym + ".initName"))
  val S2 = Scn("S2", drop = Set(Sym + ".javaBinaryNameStringCache"), add = Map(R + "Symbols$ClassSymbol" -> List(Fld(R + "Symbols$ClassSymbol", "javaBinaryNameStringCache", 4, true))))
  val S3 = Scn("S3", drop = Set(Sym + ".scala$reflect$internal$Symbols$$infos"),
    add = Map(Sym -> List(Fld(Sym, "_info", 4, true), Fld(Sym, "_infoPrev", 4, true), Fld(Sym, "_infoValidFrom", 4, false))), gone = Set(TypeHist))
  val S4 = Scn("S4", drop = Set(R + "Symbols$TermSymbol._referenced"), add = Map(R + "Symbols$ModuleSymbol" -> List(Fld(R + "Symbols$ModuleSymbol", "_referenced", 4, true))))
  val T1 = Scn("T1", drop = Set(R + "Types$TypeRef.parentsCache", R + "Types$TypeRef.parentsPeriod"))
  val T2 = Scn("T2", gone = Set("scala.collection.mutable.BitSet", "BaseTypeSeq.pending.elems"))
  val N1 = Scn("N1", drop = Set(R + "Names$Name.cachedString"))
  val X = Scn("X", drop = Set(R + "Symbols$SymbolContextApiImpl.$outer", "scala.reflect.api.Types$TypeApi.$outer", "scala.reflect.api.Names$NameApi.$outer", TypeHist + ".$outer"))
  val Base = Scn("base")
  val All = List(S1, S2, S3, S4, T1, T2, N1).reduce(_ ++ _).copy(key = "all")
  val scenarios = List(Base, S1, S2, S3, S4, (S1 ++ S2 ++ S3 ++ S4).copy(key = "S1-4"), T1, T2, N1, All, X)
  val headers = List(12, 8)

  def primSize(t: Class[_]) = t match { case java.lang.Long.TYPE | java.lang.Double.TYPE => 8; case java.lang.Integer.TYPE | java.lang.Float.TYPE => 4
    case java.lang.Short.TYPE | java.lang.Character.TYPE => 2; case java.lang.Byte.TYPE | java.lang.Boolean.TYPE => 1; case _ => 4 }
  def chain(c: Class[_]): List[Class[_]] = Iterator.iterate[Class[_]](c)(_.getSuperclass).takeWhile(_ != null).toList.reverse
  def declared(k: Class[_], s: Scn): List[Fld] =
    k.getDeclaredFields.toList.filterNot(f => Modifier.isStatic(f.getModifiers)).map(f => Fld(k.getName, f.getName, primSize(f.getType), !f.getType.isPrimitive))
      .filterNot(f => s.drop(f.key)) ++ s.add.getOrElse(k.getName, Nil)

  /** (instance size, placed fields and gaps as (offset, size, label); label "pad" for gaps). */
  def layout(c: Class[_], hdr: Int, s: Scn = Base): (Int, List[(Int, Int, String)]) = {
    var holes = List.empty[(Int, Int)] // (offset, length)
    var end = hdr
    val placed = scala.collection.mutable.ListBuffer[(Int, Int, String)]()
    def align(x: Int, a: Int) = (x + a - 1) / a * a
    for (k <- chain(c)) {
      val fs = declared(k, s)
      for (f <- fs.filter(!_.oop).sortBy(-_.size) ++ fs.filter(_.oop)) {
        val fit = holes.filter { case (o, l) => align(o, f.size) + f.size <= o + l }.sortBy(_._2).headOption
        val at = fit match {
          case Some(h @ (o, l)) =>
            val a = align(o, f.size)
            holes = holes.filterNot(_ == h) ++ List((o, a - o), (a + f.size, o + l - a - f.size)).filter(_._2 > 0)
            a
          case None =>
            val a = align(end, f.size)
            if (a > end) holes ::= ((end, a - end))
            end = a + f.size
            a
        }
        placed += ((at, f.size, f.label))
      }
    }
    val size = align(end, 8)
    val gaps = (holes ++ List((end, size - end)).filter(_._2 > 0)).map { case (o, l) => (o, l, "pad") }
    (size, (placed.toList ++ gaps).sortBy(_._1))
  }

  def histo(name: String): Map[String, (Long, Long)] = {
    val p = Paths.get(s"out/$name.tsv")
    if (!Files.exists(p)) Map() else Files.readAllLines(p).asScala.map(_.split('\t')).collect { case Array("H", c, n, b) => c -> (n.toLong, b.toLong) }.toMap
  }
  def family(c: Class[_]): Option[String] = {
    def is(n: String) = Class.forName(R + n).isAssignableFrom(c)
    if (is("Symbols$Symbol") || c.getName == TypeHist) Some("Symbol")
    else if (is("Types$Type") || is("BaseTypeSeqs$BaseTypeSeq") || c == classOf[scala.collection.mutable.BitSet]) Some("Type")
    else if (is("Names$Name")) Some("Name")
    else if (is("Scopes$Scope") || is("Scopes$ScopeEntry")) Some("Scope")
    else None
  }
  def load(n: String): Option[Class[_]] = if (n.contains("$$Lambda") || n.startsWith("[")) None else try Some(Class.forName(n)) catch { case _: Throwable => None }

  def report(corpus: String): Unit = {
    val lines = Files.readAllLines(Paths.get(s"out/$corpus.tsv")).asScala.map(_.split("\t", -1)).toList
    val h12 = histo(corpus); val h8 = histo(corpus + "-coh")
    val walkN = lines.collect { case Array("S", c, n) => c -> n.toLong }.toMap
    val heapMB = lines.collect { case Array("H", _, _, b) => b.toLong }.sum >> 20
    println(s"\n#### objects $corpus  (live heap after typer: $heapMB MB)")
    println("SCN\t" + scenarios.map(_.key).mkString("\t"))
    // class rows: R  family class heapN heapBytes12 heapBytes8 walkN  then size per (hdr, scenario)
    val classes = h12.keys.toList.flatMap(n => load(n).flatMap(c => family(c).map(f => (n, c, f)))).filter(x => !Modifier.isAbstract(x._2.getModifiers))
    val tot = scala.collection.mutable.Map[(Int, String, String), Long]().withDefaultValue(0L)
    for ((n, c, fam) <- classes.sortBy(x => -h12(x._1)._2)) {
      val (cnt, b12) = h12(n); val b8 = h8.get(n).map(_._2).getOrElse(-1L)
      val sizes = for (h <- headers; s <- scenarios) yield {
        val sz = if (s.gone(n)) 0 else layout(c, h, s)._1
        tot((h, s.key, fam)) += sz * cnt; sz
      }
      if (b12 / cnt != layout(c, 12)._1) println(s"!! MODEL MISMATCH $n actual=${b12 / cnt} model=${layout(c, 12)._1}")
      for ((n8, bb8) <- h8.get(n) if bb8 / n8 != layout(c, 8)._1) println(s"!! MODEL MISMATCH (8B) $n actual=${bb8 / n8} model=${layout(c, 8)._1}")
      println(s"R\t$fam\t$n\t$cnt\t$b12\t$b8\t${walkN.getOrElse(n, 0L)}\t${sizes.mkString("\t")}")
    }
    // owned arrays: O  family  owner.field  arrays elements bytes12 bytes8  (then per-scenario bytes, 12 then 8)
    def short(f: String) = { val parts = f.split('.'); val cls = parts(parts.length - 2).replaceAll(".*\\$", ""); cls + "." + parts.last.replaceAll(".*\\$\\$", "") }
    val owned = lines.collect { case Array("A", owner, f, a, e, b12, b8) => (short(f) + (if (f.endsWith(".pending")) ".elems" else ""), owner, a.toLong, e.toLong, b12.toLong, b8.toLong) }
      .groupBy(_._1).toList.map { case (k, xs) => (k, xs.head._2, xs.map(_._3).sum, xs.map(_._4).sum, xs.map(_._5).sum, xs.map(_._6).sum) }
    for ((k, owner, a, e, b12, b8) <- owned.sortBy(-_._5) if b12 > 10000) {
      val fam = if (k.startsWith("SymbolTable")) "Name" else load(owner).flatMap(family).getOrElse("Other")
      val by = for (h <- headers; s <- scenarios) yield { val v = if (s.gone(k)) 0L else if (h == 12) b12 else b8; tot((h, s.key, fam)) += v; v }
      println(s"O\t$fam\t$k\t$a\t$e\t${by.mkString("\t")}")
    }
    // check the modelled array bytes against the histogram for arrays that only these owners hold
    for ((arr, k) <- List("[Lscala.reflect.internal.Types$Type;" -> "BaseTypeSeq.elems", "[Lscala.reflect.internal.Scopes$ScopeEntry;" -> "Scope.hashtable"); o <- owned.find(_._1 == k))
      println(s"# array check $k: walk 12B=${o._5} histo 12B=${h12.get(arr).map(_._2)} walk 8B=${o._6} histo 8B=${h8.get(arr).map(_._2)}")
    lines.collect { case Array("N", "nameTableSize", n) => println(s"# nameTableSize\t$n") }
    for (h <- headers; fam <- List("Symbol", "Type", "Name", "Scope")) println(s"TOT\t$h\t$fam\t" + scenarios.map(s => tot((h, s.key, fam))).mkString("\t"))
    // layouts and field use for the classes holding at least 0.05% of the heap in this corpus
    val big = classes.filter(x => h12(x._1)._2 > (heapMB << 20) / 2000).sortBy(x => -h12(x._1)._2)
    for ((n, c, _) <- big; h <- headers) println(s"L\t$n\t$h\t" + layout(c, h)._2.map { case (o, sz, l) => s"$o:$sz:$l" }.mkString(";"))
    for ((n, c, _) <- big; l <- lines) l match {
      case Array("F", `n`, f, t, nd, sen) => println(s"U\t$n\t${short(f)}\t${t.replaceAll(".*[.$]", "")}\t$nd\t${walkN(n)}\t$sen")
      case _ =>
    }
  }
}
