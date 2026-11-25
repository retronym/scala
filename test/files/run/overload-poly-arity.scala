//> using options -Xlint:-adapted-args

// Overload resolution among polymorphic alternatives whose arity differs from
// the argument count: `Infer.isApplicable` rejects those on arity before cloning
// their type params, which must not change which alternative is chosen.
object Test {
  object O {
    def f[A](a: A): String = "f1"
    def f[A, B](a: A, b: B, c: Int): String = "f3"

    def g[A](a: A, b: Int = 0): String = "g-default"
    def g[A](a: A, b: A, c: A): String = "g3"

    def h[A](a: A, as: A*): String = "h-varargs"
    def h[A](a: A, b: String, c: String, d: String): String = "h4"

    def k[A](x: A, y: Int): String = "k2"
    def k[A](x: A): String = "k1"

    def t[A](p: (A, A)): String = "t-tupled"
    def t[A](a: A, b: A, c: A): String = "t3"

    def u[A](as: A*): String = "u-varargs"
    def u[A](a: A, b: A): String = "u2"
  }
  import O._

  def main(args: Array[String]): Unit = {
    println(f(1))
    println(f(1, 2, 3))
    println(g(1))
    println(g(1, 2))
    println(g(1, b = 2))
    println(g(1, 2, 3))
    println(h(1))
    println(h(1, 2, 3, 4, 5))
    println(h(1, "a", "b", "c"))
    println(h(1, Seq(2, 3): _*))
    println(k(y = 1, x = "a"))
    println(k(x = "a"))
    println(t(1, 2))
    println(t(1, 2, 3))
    println(u())
    println(u(1))
    println(u(1, 2))
    println(u(1, 2, 3))
  }
}
