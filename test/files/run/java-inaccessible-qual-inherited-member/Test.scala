import p._

// The members are inherited from accessible classes. The static type of the qualifier, `p.Hid`,
// is not accessible here and must not be the receiver class in the bytecode.
object Test {
  def t(name: String)(f: => Any): Unit =
    try println(name + "=" + f) catch { case e: Throwable => println(name + " throws " + e.getClass.getName) }

  def main(args: Array[String]): Unit = {
    val x = new Pub
    t("method of public Base")(x.self().b())
    t("field of public Base")(x.self().bf)
    t("field assignment")({ x.self().bf = 3; x.bf })
    t("hashCode")(x.self().hashCode == x.hashCode)
    t("toString")(x.self().toString == x.toString)
    t("explicit upcast")(x.self().asInstanceOf[Base].b())
  }
}
