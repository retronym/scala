// The specialized subclass `C$mcI$sp` used to get `override def f(x: Int) = f$mcI$sp(x)`, which also
// overrode the user's overload `f(x: Int)`, so calls to that overload ran the body of `f(x: T)`.

class C[@specialized(Int) T] {
  def f(x: T): String = "generic-T"
  def f(x: Int): String = "Int-overload"
  def g(t: T): String = this.f(t)
  def k: String = this.f(42)
}

class Base { def f(x: Int): String = "Base-Int" }
class D[@specialized(Int) T] extends Base {
  def f(x: T): String = "D-T"
  def k: String = this.f(42)
}

// no clash: the forwarder `f(x: Int)` is still needed
class E[@specialized(Int) T] {
  def f(x: T): String = "E-T"
  def f(x: Long): String = "E-Long"
}

object Test {
  def h[T](c: C[T]) = c.f(1)
  def gg[T](c: C[T], t: T) = c.g(t)
  def hd[T](d: D[T]) = d.f(1)
  def he[T](e: E[T], t: T) = (e.f(t), e.f(1L))

  def main(args: Array[String]): Unit = {
    println(List(h(new C[String]), h(new C[Int]), new C[String].k, new C[Int].k))
    println(List(gg(new C[String], ""), gg(new C[Int], 1), new C[Int].g(1)))
    println(List(hd(new D[String]), hd(new D[Int]), new D[Int].k, (new D[Int]: Base).f(1)))
    println(List(he(new E[Int], 1), he(new E[String], "s"), new E[Int].f(1)))
  }
}
