// The receiver of a call with named arguments is evaluated before the arguments (SLS 6.6, 6.6.1),
// also when it is a lazy val or an object.
object Test {
  val log = new collection.mutable.ArrayBuffer[String]
  def t[A](s: String, a: A): A = { log += s; a }
  def show(name: String)(body: => Any): Unit = {
    log.clear()
    val r = try body.toString catch { case e: Throwable => e.getClass.getSimpleName }
    println(s"$name: ${log.mkString(" ")} -> $r")
  }
  class C { def f(a: Int, b: Int, c: Int = t("default-c", 0)): String = s"a=$a b=$b" }
  var counter = 0
  def next(): Int = { counter += 1; counter }
  lazy val lz1: C = t("lz1", new C)
  lazy val lz2: C = t("lz2", { counter = 100; new C })
  object O1 extends C { t("O1", ()) }
  object O2 extends C { t("O2", ()) }
  def mk(): C = t("mk", new C)

  def main(args: Array[String]): Unit = {
    show("positional, lazy val receiver ")(lz1.f(t("a", 1), t("b", 2)))
    show("named, method receiver        ")(mk().f(b = t("b", 2), a = t("a", 1)))
    show("named, lazy val receiver      ")(lz2.f(b = t("b", next()), a = t("a", next())))
    show("positional, object receiver   ")(O1.f(t("a", 1), t("b", 2)))
    show("named, object receiver        ")(O2.f(b = t("b", 2), a = t("a", 1)))
  }
}
