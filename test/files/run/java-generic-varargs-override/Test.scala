// Overrides of Java varargs methods whose parameter types mention a type parameter must override
// the erased Java signature, `g(Object[])` for `g(T... xs)`, also when called from Java.

class SG2 extends JV[String] {
  override def g(xs: String*): String = "SG2.g" + xs.mkString
  override def m[U](xs: U*): String = "SG2.m" + xs.mkString
  override def r(xs: String*): String = "SG2.r" + xs.mkString
  override def h(a: String, xs: String*): String = "SG2.h" + a + xs.mkString
}
// overridden again
class SG3 extends SG2 { override def g(xs: String*): String = "SG3.g" + xs.mkString }
// primitive type argument
class SGI extends JV[Int] { override def g(xs: Int*): String = "SGI.g" + xs.sum }
// bounded type parameter, erases to CharSequence[]
class SB extends JB[String] { override def g(xs: String*): String = "SB.g" + xs.mkString }
// method-level type parameter next to a class-level one
class SW extends JW[String] { override def k[U](t: String, us: U*): String = "SW.k" + t + us.mkString }
// implementing an abstract method
class SA extends JA[String] { def a(xs: String*): String = "SA.a" + xs.mkString }
// a trait in between
trait TI extends JI[String] { override def g(xs: String*): String = "TI.g" + xs.mkString }
class CI extends TI
trait TC extends JV[String] { override def g(xs: String*): String = "TC.g" + xs.mkString }
class CC extends JV[String] with TC
class CC2 extends TC

object Test {
  def t(name: String)(body: => Any): Unit =
    try println(s"$name: $body") catch { case e: Throwable => println(s"$name threw $e") }

  def main(args: Array[String]): Unit = {
    val s = new SG2
    t("SG2 g via JV")((s: JV[String]).g("a", "b"))
    t("SG2 g from Java")(s.callG1("q"))
    t("SG2 g Java caller")(JavaCaller.sg2(s))
    t("SG2 m via JV")((s: JV[String]).m[Int](1, 2))
    t("SG2 m from Java")(s.callM("q"))
    t("SG2 r via JV")((s: JV[String]).r("a"))
    t("SG2 r from Java")(s.callR("q"))
    t("SG2 h from Java")(s.callH("q"))
    t("SG3 g via JV")((new SG3: JV[String]).g("a"))
    t("SG3 g via SG2")((new SG3: SG2).g("a"))
    t("SG3 g from Java")(new SG3().callG1("q"))
    t("SGI g via JV")((new SGI: JV[Int]).g(1, 2))
    t("SGI g from Java")(new SGI().callG1(5))
    t("SB g via JB")((new SB: JB[String]).g("a"))
    t("SB g from Java")(new SB().callG1("q"))
    t("SB g from Java, non-generic call")(new SB().callGS("q"))
    t("SB g Java caller")(JavaCaller.sb(new SB))
    t("SW k via JW")((new SW: JW[String]).k[Int]("t", 1, 2))
    t("SW k from Java")(new SW().callK("q"))
    t("SA a via JA")((new SA: JA[String]).a("x"))
    t("SA a from Java")(new SA().callA("q"))
    t("CI g via JI")((new CI: JI[String]).g("a"))
    t("CI g from Java")(JI.call(new CI, "q"))
    t("CC g via JV")((new CC: JV[String]).g("a"))
    t("CC g from Java")(new CC().callG1("q"))
    t("CC2 g via JV")((new CC2: JV[String]).g("a"))
    t("CC2 g from Java")(new CC2().callG1("q"))
  }
}
