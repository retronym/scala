class VU(val u: Unit) extends AnyVal { def show = "VU(" + u + ")" }
class VI(val i: Int) extends AnyVal { def show = "VI(" + i + ")" }
class Gen[T](val x: T)
object HI extends Gen[VI](new VI(1)) { println("init HI") }
object HU extends Gen[VU](new VU(())) { println("init HU") }
object Test {
  def t(s: String)(body: => Any): Unit =
    try println(s + ": " + body) catch { case e: Throwable => println(s + ": threw " + e.getClass.getName) }
  def main(args: Array[String]): Unit = {
    val str: Any = "str"
    // control: a value class over Int
    t("str.asInstanceOf[VI]")(str.asInstanceOf[VI].show)
    val li = List[Any]("x").asInstanceOf[List[VI]]
    t("List[VI].head")(li.head.show)
    // a value class over Unit: the cast is not checked
    t("str.asInstanceOf[VU]")(str.asInstanceOf[VU].show)
    val lu = List[Any]("x").asInstanceOf[List[VU]]
    t("List[VU].head")(lu.head.show)
    def id[T](x: Any): T = x.asInstanceOf[T]
    t("id[VI](str)")(id[VI](str).show)
    t("id[VU](str)")(id[VU](str).show)
    // the unboxed tree itself is dropped when it is a stable path: HU is never initialised
    t("HI.x")(HI.x.show)
    t("HU.x")(HU.x.show)
  }
}
