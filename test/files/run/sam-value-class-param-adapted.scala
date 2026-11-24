case class VI(x: Int) extends AnyVal
case class VS(s: String) extends AnyVal
case class VT[T](t: T) extends AnyVal

trait A1[R] { def ap(a: VI): R }                // value-class parameter, generic result
trait A2[R] { def ap(a: VS): R }
trait A3[B] { def ap(a: VS, b: B): String }     // value-class parameter next to a generic parameter
trait A4[A] { def ap(a: VT[A]): VT[A] }         // generic value class

object Test {
  def t(name: String)(body: => Any): Unit =
    try println(name + ": " + body)
    catch { case e: ClassCastException => println(name + ": ClassCastException") }

  def main(args: Array[String]): Unit = {
    t("A1[Int]")    { val f: A1[Int] = v => v.x + 1;            f.ap(VI(1)) }
    t("A1[Unit]")   { val f: A1[Unit] = v => ();                f.ap(VI(1)) }
    t("A1[VI]")     { val f: A1[VI] = v => v;                   f.ap(VI(1)) }
    t("A2[Int]")    { val f: A2[Int] = v => v.s.length;         f.ap(VS("ab")) }
    t("A2[String]") { val f: A2[String] = v => v.s;             f.ap(VS("ab")) }
    t("A3[Int]")    { val f: A3[Int] = (v, i) => v.s * i;       f.ap(VS("ab"), 2) }
    t("A4[Int]")    { val f: A4[Int] = v => VT(v.t + 1);        f.ap(VT(1)) }
    t("anon class") { val f: A1[Int] = new A1[Int] { def ap(v: VI) = v.x + 1 }; f.ap(VI(1)) }
  }
}
