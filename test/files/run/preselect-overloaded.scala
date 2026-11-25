// Pins the alternative chosen by overloading resolution in cases where
// Typers.preSelectOverloaded filters alternatives on argument "shapes"
// (Nothing, except function literals, partial functions and named args)
// before inferMethodAlternative sees the real argument types.

object Arity {
  def f(): String = "f()"
  def f(a: Int): String = "f(Int)"
  def f(a: Int, b: Int): String = "f(Int,Int)"
  def f(a: Int, b: Int, c: Int): String = "f(Int,Int,Int)"
}

object Defaults {
  def f(a: Int): String = "f(Int)"
  def f(a: String, b: Int = 1): String = "f(String,Int=)"
  def g(a: Int, b: Int = 2): String = "g(Int,Int=)"
  def g(a: String): String = "g(String)"
}

object Varargs {
  def f(a: Int): String = "f(Int)"
  def f(a: Int, as: Int*): String = "f(Int,Int*)"
  def g(as: Int*): String = "g(Int*)"
  def g(a: String, b: String): String = "g(String,String)"
  def h(a: Int, as: Int*): String = "h(Int,Int*)"
  def h(a: Int, b: Int): String = "h(Int,Int)"
}

object Tupling {
  def f(t: (Int, Int)): String = "f((Int,Int))"
  def f(a: String): String = "f(String)"
  def g(a: Any): String = "g(Any)"
  def g(a: Int, b: Int, c: Int): String = "g(Int,Int,Int)"
}

object Named {
  def f(a: Int, b: String): String = "f(a:Int,b:String)"
  def f(x: Int, y: Int): String = "f(x:Int,y:Int)"
  def g(a: Int, b: Int = 0): String = "g(a,b=)"
  def g(c: String): String = "g(c)"
}

object Functions {
  def f(fn: Int => Int): String = "f(Int=>Int)"
  def f(fn: (Int, Int) => Int): String = "f((Int,Int)=>Int)"
  def f(fn: Int): String = "f(Int)"
  def g(pf: PartialFunction[Int, Int]): String = "g(PF)"
  def g(s: String): String = "g(String)"
  def h(fn: () => Int): String = "h(()=>Int)"
  def h(fn: Int => Int, x: Int): String = "h(Int=>Int,Int)"
  def sam(r: Runnable): String = "sam(Runnable)"
  def sam(s: String): String = "sam(String)"
}

object ExpectedType {
  def f(a: Int): Int = 1
  def f(a: String): String = "f(String):String"
  def g[T](a: T): List[T] = List(a)
  def g(a: Int, b: Int): String = "g(Int,Int):String"
  def h(a: Any): String = "h(Any):String"
  def h(a: Int): Unit = ()
}

object Poly {
  def f[T <: AnyVal](a: T): String = "f[T<:AnyVal]"
  def f(a: String): String = "f(String)"
  def g[A](a: A, b: List[A]): String = "g[A](A,List[A])"
  def g[A](a: A): String = "g[A](A)"
}

class Box {
  def apply(a: Int): String = "Box.apply(Int)"
}

class Applies {
  // a polymorphic nullary alternative whose result has an `apply`: followApply must still look it up
  def p[T]: Box = new Box
  def p(a: String): String = "p(String)"

  object o {
    def apply(a: Int): String = "o.apply(Int)"
    def apply(a: Int, b: Int): String = "o.apply(Int,Int)"
  }
  val v: String = "v"
  def v(a: Int): String = "v(Int)"
}

object Test {
  def main(args: Array[String]): Unit = {
    println(Arity.f())
    println(Arity.f(1))
    println(Arity.f(1, 2))
    println(Arity.f(1, 2, 3))

    println(Defaults.f(1))
    println(Defaults.f("a"))
    println(Defaults.f("a", 2))
    println(Defaults.g(1))
    println(Defaults.g("a"))

    println(Varargs.f(1))
    println(Varargs.f(1, 2))
    println(Varargs.f(1, Seq(2, 3): _*))
    println(Varargs.g())
    println(Varargs.g(1, 2, 3))
    println(Varargs.g("a", "b"))
    println(Varargs.g(Seq(1): _*))
    println(Varargs.h(1, 2))
    println(Varargs.h(1, 2, 3))
    println(Varargs.h(1))

    println(Tupling.f((1, 2)))
    println(Tupling.f(1, 2))
    println(Tupling.f("a"))
    println(Tupling.g(1, 2, 3))

    println(Named.f(a = 1, b = "x"))
    println(Named.f(y = 2, x = 1))
    println(Named.g(a = 1))
    println(Named.g(c = "x"))

    println(Functions.f(x => x + 1))
    println(Functions.f((x, y) => x + y))
    println(Functions.f(_ + 1))
    println(Functions.f(1))
    println(Functions.g { case x => x })
    println(Functions.g("a"))
    println(Functions.h(() => 1))
    println(Functions.h(x => x, 1))
    println(Functions.sam(() => ()))
    println(Functions.sam("a"))

    val s: String = ExpectedType.f("a")
    println(s)
    val g1: String = ExpectedType.g(1, 2)
    println(g1)
    val g2: List[Int] = ExpectedType.g(1)
    println(g2)
    val h: String = ExpectedType.h(1)
    println(h)

    println(Poly.f(1))
    println(Poly.f("a"))
    println(Poly.g(1, List(2)))
    println(Poly.g(1))

    val a = new Applies
    println(a.o(1))
    println(a.o(1, 2))
    println(a.v(1))
    println(a.v)
    println(a.p(1))
    println(a.p("a"))
  }
}
