object Test {
  def t(name: String)(f: => Any): Unit = {
    @volatile var got = "<no result after 3s>"
    val th = new Thread(() => got = try String.valueOf(f) catch { case e: Throwable => e.getClass.getSimpleName }); th.setDaemon(true); th.start(); th.join(3000)
    println(f"$name%-74s = $got")
  }
  def main(a: Array[String]): Unit = {
    def bd(s: String) = BigDecimal(s)
    t("Range.BigDecimal.inclusive(0.34, -0.12, -0.01).length   (control)")(Range.BigDecimal.inclusive(bd("0.34"), bd("-0.12"), bd("-0.01")).length)
    t("Range.BigDecimal.inclusive(0.34, -21474835.00, -0.01).length   (control)")(Range.BigDecimal.inclusive(bd("0.34"), bd("-21474835.00"), bd("-0.01")).length)
    t("Range.BigDecimal.inclusive(0.34, -21474836.11, -0.01).length")(Range.BigDecimal.inclusive(bd("0.34"), bd("-21474836.11"), bd("-0.01")).length)
    t("Range.BigDecimal.inclusive(0.34, -21474836.12, -0.01).length")(Range.BigDecimal.inclusive(bd("0.34"), bd("-21474836.12"), bd("-0.01")).length)
    val d = Range.BigDecimal.inclusive(bd("0.34"), bd("-245480424064.33"), bd("-0.01"))
    t("d = Range.BigDecimal.inclusive(0.34, -245480424064.33, -0.01); d.take(Int.MaxValue).length")(d.take(Int.MaxValue).length)
    t("d.take(Int.MaxValue).last")(d.take(Int.MaxValue).last)
    t("d.take(1000).length   (control)")(d.take(1000).length)
    System.exit(0)
  }
}
