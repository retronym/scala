import scala.collection.immutable.NumericRange
object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    val b = NumericRange.inclusive[Byte](-128, 57, 5)
    t("b = NumericRange.inclusive[Byte](-128, 57, 5); b.length")(b.length)
    t("b.iterator.knownSize")(b.iterator.knownSize)
    val b2 = NumericRange.inclusive[Byte](-128, 126, 127)
    t("b2 = NumericRange.inclusive[Byte](-128, 126, 127); b2.toList")(b2.toList)
    t("b2.iterator.knownSize")(b2.iterator.knownSize)
    t("b2.iterator.toVector")(b2.iterator.toVector)
    val s = NumericRange[Short](29199, -32768, -6025)
    t("s = NumericRange[Short](29199, -32768, -6025); s.length")(s.length)
    t("s.iterator.knownSize")(s.iterator.knownSize)
    t("s.iterator.drop(1).toList.size")(s.iterator.drop(1).toList.size)
    val l = NumericRange.inclusive(Long.MinValue, Long.MaxValue, Long.MaxValue)
    t("l = NumericRange.inclusive(Long.MinValue, Long.MaxValue, Long.MaxValue); l.length")(l.length)
    t("l.iterator.knownSize")(l.iterator.knownSize)
    t("l.iterator.toVector.size")(l.iterator.toVector.size)
    System.exit(0)
  }
}
