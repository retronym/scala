object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    val r = Int.MinValue to 51
    t("r = Int.MinValue to 51; r.contains(3)")(r.contains(3))
    t("r.lastIndexOf(3, 1073741823)")(r.lastIndexOf(3, 1073741823))
    t("r.indexOf(3, Int.MinValue)")(r.indexOf(3, Int.MinValue))
    t("r.indexOf(3)")(r.indexOf(3))
    val d = 544 until Int.MinValue by -1
    t("(544 until Int.MinValue by -1).lastIndexOf(-2147483646, Int.MaxValue)")(d.lastIndexOf(-2147483646, Int.MaxValue))
    System.exit(0)
  }
}
