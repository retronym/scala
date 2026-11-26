object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 1s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(1000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    val e = Int.MinValue until Int.MinValue
    t("e = Int.MinValue until Int.MinValue; e.isEmpty")(e.isEmpty)
    t("e.iterator.drop(1).hasNext")(e.iterator.drop(1).hasNext)
    t("e.iterator.drop(1).take(3).toList")(e.iterator.drop(1).take(3).toList)
    t("e.indexWhere(_ => true, 1)")(e.indexWhere(_ => true, 1))
    t("e.segmentLength(_ => true, 1)")(e.segmentLength(_ => true, 1))
    val f = 4 until Int.MaxValue by -1
    t("f = 4 until Int.MaxValue by -1; f.isEmpty")(f.isEmpty)
    t("f.iterator.drop(2).hasNext")(f.iterator.drop(2).hasNext)
    t("f.segmentLength(_ != 0, 2)")(f.segmentLength(_ != 0, 2))
    t("(Int.MaxValue until Int.MaxValue by -1).iterator.drop(1).hasNext")((Int.MaxValue until Int.MaxValue by -1).iterator.drop(1).hasNext)
    t("(1 to 3).take(0).iterator.drop(1).hasNext   (control)")((1 to 3).take(0).iterator.drop(1).hasNext)
    System.exit(0)
  }
}
