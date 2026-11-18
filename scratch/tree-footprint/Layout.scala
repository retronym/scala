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
  }
}
