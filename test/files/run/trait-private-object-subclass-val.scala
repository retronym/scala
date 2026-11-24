// A private object in a trait is not inherited, so a member of the same name in a subclass
// neither overrides nor conflicts with it. The subclass must still implement the trait's module accessor.
trait T {
  private object f { override def toString = "T.f" }
  def g: String = f.toString
}
class C1 extends T { val f: String = "C1.f" }
class C2 extends T { lazy val f: String = "C2.f" }
class C3 extends T { var f: String = "C3.f" }
class C4 extends T { def f: String = "C4.f" }
class C5 extends T { object f { override def toString = "C5.f" } }
case class C6(f: String) extends T

trait U extends T { val f: String = "U.f" }
class C7 extends U

trait TL {
  private[this] object f { override def toString = "TL.f" }
  def g: String = f.toString
}
class C8 extends TL { val f: String = "C8.f" }

trait TC {
  private case object f
  def g: String = f.toString
}
class C9 extends TC { val f: String = "C9.f" }

object Test {
  def main(args: Array[String]): Unit = {
    val c1 = new C1; println((c1.g, c1.f))
    val c2 = new C2; println((c2.g, c2.f))
    val c3 = new C3; println((c3.g, c3.f))
    val c4 = new C4; println((c4.g, c4.f))
    val c5 = new C5; println((c5.g, c5.f))
    val c6 = C6("C6.f"); println((c6.g, c6.f))
    val c7 = new C7; println((c7.g, c7.f))
    val c8 = new C8; println((c8.g, c8.f))
    val c9 = new C9; println((c9.g, c9.f))
  }
}
