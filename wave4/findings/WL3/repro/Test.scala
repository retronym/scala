import scala.jdk.CollectionConverters._
import scala.collection.{mutable => mu, immutable => im}
import scala.util.Try
object Test {
  def show(what: String, r: => Any): Unit = println(what + ": " + Try(r).fold(e => e.getClass.getSimpleName, x => String.valueOf(x)))
  def main(args: Array[String]): Unit = {
    show("mutable.BitSet(0, 5).asJava.contains(null)", mu.BitSet(0, 5).asJava.contains(null))
    show("immutable.BitSet(0, 5).asJava.contains(null)", im.BitSet(0, 5).asJava.contains(null))
    show("mutable.HashSet[Int](0, 5).asJava.contains(null)", mu.HashSet[Int](0, 5).asJava.contains(null))
    show("immutable.TreeSet[Int](0, 5).asJava.contains(null)", im.TreeSet[Int](0, 5).asJava.contains(null))
    show("mutable.TreeSet[Int](0, 5).asJava.contains(null)", mu.TreeSet[Int](0, 5).asJava.contains(null))
    show("Set[Int](0, 5).asJava.contains(null)", Set[Int](0, 5).asJava.contains(null))
    show("Vector[Int](0, 5).asJava.contains(null)", Vector[Int](0, 5).asJava.contains(null))
    show("Vector[Int](0, 5).asJava.indexOf(null)", Vector[Int](0, 5).asJava.indexOf(null))
    show("ArrayBuffer[Int](0, 5).asJava.remove(null: Object)", { val b = mu.ArrayBuffer[Int](0, 5); (b.asJava.remove(null: Object), b) })
    val bs = mu.BitSet(0, 5); show("mutable.BitSet(0, 5).asJava.remove(null)", (bs.asJava.remove(null), bs))
    show("mutable.LongMap(0L -> a).asJava.get(null)", mu.LongMap(0L -> "a").asJava.get(null))
    show("mutable.LongMap(0L -> a).asJava.containsKey(null)", mu.LongMap(0L -> "a").asJava.containsKey(null))
    show("immutable.IntMap(0 -> a).asJava.get(null)", im.IntMap(0 -> "a").asJava.get(null))
    show("Map[Int,String](0 -> a).asJava.get(null)", Map[Int, String](0 -> "a").asJava.get(null))
    show("mutable.HashMap[Int,String](0 -> a).asJava.get(null)", mu.HashMap[Int, String](0 -> "a").asJava.get(null))
    show("immutable.TreeMap[Int,String](0 -> a).asJava.get(null)", im.TreeMap[Int, String](0 -> "a").asJava.get(null))
    val lm = mu.LongMap(0L -> "a"); show("mutable.LongMap(0L -> a).asJava.remove(null)", (lm.asJava.remove(null), lm))
  }
}
