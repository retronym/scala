// `C1` and `C1$mcI$sp` each declare the super accessor `super$f`. The two have the same erased
// type, `()Function0`, and a class that extends `C1$mcI$sp` implements both.
trait B[@specialized(Int) T] {
  def x: T
  def f: () => T = () => x
  def g(t: T): (T, T) = (t, x)
}
trait C1[@specialized(Int) U] extends B[U] {
  override def f: () => U = super.f
  override def g(u: U): (U, U) = super.g(u).swap
}
class C2 extends C1[Int] { def x = 1 }
class C3 extends C1[String] { def x = "s" }
class C4[@specialized(Int) V](val x: V) extends C1[V]
object Test {
  def main(args: Array[String]): Unit = {
    println(new C3().f())
    println(new C2().f())
    println(new C2().g(2))
    println(new C4(3).f())
    println(new C4("t").g("u"))
  }
}
