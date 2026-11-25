import scala.language.implicitConversions

class Foo(val s: String)

object Test {
  implicit def f(): Foo = new Foo("f()")
  implicit def g(x: Int): Foo = new Foo(s"g($x)")

  def thunk(implicit t: () => Foo) = t().s

  def main(args: Array[String]): Unit = {
    val h0: () => Foo = f
    val h1: Int => Foo = g
    println(h0().s)
    println(h1(1).s)
    // An implicit method is eligible for a function type of its arity, including arity zero.
    println(implicitly[Int => Foo].apply(2).s)
    println(implicitly[() => Foo].apply().s)
    println(thunk)
  }
}
