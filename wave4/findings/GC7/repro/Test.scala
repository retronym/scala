import scala.collection.mutable
object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    t("HashSet(1, 2, 3).addAll(s.view.map(_ + 1)).size") { val s = mutable.HashSet(1, 2, 3); s.addAll(s.view.map(_ + 1)).size }
    t("HashSet(1, 2, 3).subtractAll(s.view.map(_ + 1))") { val s = mutable.HashSet(1, 2, 3); s.subtractAll(s.view.map(_ + 1)) }
    t("TreeSet(1 to 40).subtractAll(s.view.map(_ + 1))") { val s = mutable.TreeSet.from(1 to 40); s.subtractAll(s.view.map(_ + 1)) }
    t("LinkedHashSet(1 to 40).subtractAll(s.view.map(_ + 1)).size") { val s = mutable.LinkedHashSet.from(1 to 40); s.subtractAll(s.view.map(_ + 1)).size }
    t("HashMap(1->2, 2->3, 3->4).addAll(m.view.map { case (k, v) => (k + 1, v + 10) }).size") { val m = mutable.HashMap(1 -> 2, 2 -> 3, 3 -> 4); m.addAll(m.view.map { case (k, v) => (k + 1, v + 10) }).size }
    t("TreeMap(1->2, 2->3, 3->4).addAll(same)") { val m = mutable.TreeMap(1 -> 2, 2 -> 3, 3 -> 4); m.addAll(m.view.map { case (k, v) => (k + 1, v + 10) }) }
    t("LinkedHashMap(1->2, 2->3, 3->4).addAll(same)") { val m = mutable.LinkedHashMap(1 -> 2, 2 -> 3, 3 -> 4); m.addAll(m.view.map { case (k, v) => (k + 1, v + 10) }) }
    t("LongMap(1->2, 2->3, 3->4).addAll(same).size") { val m = mutable.LongMap(1L -> 2, 2L -> 3, 3L -> 4); m.addAll(m.view.map { case (k, v) => (k + 1, v + 10) }).size }
    t("HashMap(1->2, 2->3, 3->4).subtractAll(m.keySet.view.map(_ + 1))") { val m = mutable.HashMap(1 -> 2, 2 -> 3, 3 -> 4); m.subtractAll(m.keySet.view.map(_ + 1)) }
    t("ArrayBuffer(1, 2, 3).flatMapInPlace(_ => b.view.take(2))") { val b = mutable.ArrayBuffer(1, 2, 3); b.flatMapInPlace(_ => b.view.take(2)) }
    t("ListBuffer(1, 2, 3).flatMapInPlace(_ => b.view.take(2))   (control)") { val b = mutable.ListBuffer(1, 2, 3); b.flatMapInPlace(_ => b.view.take(2)) }
    System.exit(0)
  }
}
