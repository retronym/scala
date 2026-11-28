object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    val r = Int.MinValue to Int.MaxValue by Int.MaxValue
    t("r = Int.MinValue to Int.MaxValue by Int.MaxValue; r.length")(r.length)
    t("r.toList")(r.toList)
    t("r.iterator.knownSize")(r.iterator.knownSize)
    t("r.iterator.toVector")(r.iterator.toVector)
    t("r.iterator.to(ArrayBuffer)")(r.iterator.to(scala.collection.mutable.ArrayBuffer))
    val s = Range(-1073741826, Int.MaxValue, 715827883)
    t("s = -1073741826 until Int.MaxValue by 715827883; s.length")(s.length)
    t("s.iterator.knownSize")(s.iterator.knownSize)
    t("s.iterator.hasNext")(s.iterator.hasNext)
    t("s.iterator.toList")(s.iterator.toList)
    t("s.iterator.map(_ / 2).mkString(\",\")")(s.iterator.map(_ / 2).mkString(","))
    t("(Int.MaxValue to Int.MinValue by -Int.MaxValue).iterator.knownSize")((Int.MaxValue to Int.MinValue by -Int.MaxValue).iterator.knownSize)
    System.exit(0)
  }
}
