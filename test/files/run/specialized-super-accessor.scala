// A super call in a trait to a method with a specialized method type parameter.

trait T0[A] {
  def m[@specialized(Int) B](p: B): Int = 1
}
trait T1[A] extends T0[A] {
  override def m[@specialized(Int) B](p: B): Int = super.m[B](p) + 1
}
class C extends T1[String]

trait T2 extends T0[String] {
  def n[@specialized(Int) B](p: B): Int = super.m(p) + 10
}
class D extends T2

class Show {
  def show[@specialized(Int) A](a: A): String = a.toString
}
trait Quoted extends Show {
  override def show[@specialized(Int) A](a: A): String = "'" + super.show(a) + "'"
}
class QS extends Show with Quoted

// A super call from a lambda in a class also goes through a super accessor.
class X extends Show {
  override def show[@specialized(Int) A](a: A): String = "x"
  def f = () => super.show(1)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().m(1))
    println(new C().m("s"))
    println(new D().n(1))
    println(new QS().show(1))
    println(new QS().show("s"))
    println(new X().f())
  }
}
