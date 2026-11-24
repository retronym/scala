// The default getter of `x: T = 1` has the literal type `1`. With an expected type to which `Int` widens, the
// application infers `T = Int` and widens the result, rather than passing the `Int` default where a `Long` is due.
// On 2.13.18 `val l: Long = f()` threw ClassCastException (java.lang.Integer cannot be cast to java.lang.Long).
object Test {
  def f[T](x: T = 1) = x
  def g[T](x: T = 'a') = x
  def h[T](a: Int)(x: T = 1) = x

  def main(args: Array[String]): Unit = {
    val l: Long = f()
    val d: Double = f()
    val c: Int = g()
    val lh: Long = h(0)()
    val ln: Long = f(x = 2)
    val one: 1 = f()
    val any: Any = f()
    println(List[Any](l, d, c, lh, ln, one, any).map(x => s"$x ${x.getClass.getSimpleName}").mkString(", "))
  }
}
