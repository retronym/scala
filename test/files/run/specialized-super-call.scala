// A super call from a subclass that fixes a specialized type parameter must call the
// specialized parent's implementation, not dispatch back to the overriding method.

class B[@specialized(Int) T] { def m(t: T): T = t }
class I extends B[Int] { override def m(t: Int) = super.m(t) + 1 }
class J extends I { override def m(t: Int) = super.m(t) + 10 }
class K extends B[Int] {
  override def m(t: Int) = t * 100
  def viaSuper(t: Int) = super.m(t)
}

trait TB[@specialized(Int) T] { def m(t: T): T = t }
class TI extends TB[Int] { override def m(t: Int) = super.m(t) + 1 }

class B2[@specialized(Int) A, @specialized(Int) Z] { def m(a: A, z: Z): A = a }
class I2 extends B2[Int, Int] { override def m(a: Int, z: Int) = super.m(a, z) + z }

class B3[@specialized(Int) T] { def m[@specialized(Int) U](t: T, u: U): T = t }
class I3 extends B3[Int] { override def m[@specialized(Int) U](t: Int, u: U) = super.m[U](t, u) + 1 }

class G[@specialized(Int) T] extends TB[T] { override def m(t: T) = super.m(t) }

object Test {
  def t(label: String)(f: => Any): Unit =
    try println(s"$label: $f")
    catch { case _: StackOverflowError => println(s"$label: StackOverflowError") }

  def main(args: Array[String]): Unit = {
    t("class")(new I().m(1))
    t("class, generic call site")((new I(): B[Int]).m(1))
    t("two levels")(new J().m(1))
    t("other method")(new K().viaSuper(1))
    t("trait")(new TI().m(1))
    t("anonymous")(new B[Int] { override def m(t: Int) = super.m(t) + 1 }.m(1))
    t("two type params")(new I2().m(1, 2))
    t("specialized method")(new I3().m(1, 2))
    t("specialized method, reference arg")(new I3().m(1, "s"))
    t("generic subclass, String")(new G[String]().m("s"))
  }
}
