import scala.collection.concurrent.TrieMap
import scala.util.Try
object Test {
  def show(what: String, r: => Any): Unit = println(what + ": " + Try(r).fold(e => e.getClass.getName, x => x.toString))
  def main(args: Array[String]): Unit = {
    val m = TrieMap(1 -> "a", 2 -> "b")
    val ro = m.readOnlySnapshot().asInstanceOf[TrieMap[Int, String]]
    show("ro.size", ro.size)
    show("ro.readOnlySnapshot()", ro.readOnlySnapshot().toList.sorted)
    show("ro.snapshot()", ro.snapshot().toList.sorted)
    show("ro.clone()", ro.clone().toList.sorted)
  }
}
