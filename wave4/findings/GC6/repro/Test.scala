import scala.collection.mutable
object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    t("HashMap(1 -> 2): (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m)") { val m = mutable.HashMap(1 -> 2); (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m) }
    t("HashMap(1 -> 2): (m.updateWith(1)(_ => { m.clear(); Some(7) }), m)") { val m = mutable.HashMap(1 -> 2); (m.updateWith(1)(_ => { m.clear(); Some(7) }), m) }
    t("LinkedHashMap(1 -> 2): (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m)") { val m = mutable.LinkedHashMap(1 -> 2); (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m) }
    t("LinkedHashMap(1 -> 2): then (m.size, m.contains(1))") { val m = mutable.LinkedHashMap(1 -> 2); m.updateWith(1)(_ => { m.remove(1); Some(7) }); (m.size, m.contains(1)) }
    t("TreeMap(1 -> 2): (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m)   (control)") { val m = mutable.TreeMap(1 -> 2); (m.updateWith(1)(_ => { m.remove(1); Some(7) }), m) }
    t("HashMap(1 -> 2): (m.updateWith(1)(_ => { m.put(9, 9); Some(7) }), m)   (control)") { val m = mutable.HashMap(1 -> 2); (m.updateWith(1)(_ => { m.put(9, 9); Some(7) }), m) }
    System.exit(0)
  }
}
