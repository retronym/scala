class V1(val x: Int) extends AnyVal { def f = x }
object V1 { val f$extension = 42 }

class V2(val x: Int) extends AnyVal { def f(a: Int) = x }
object V2 { def f$extension(a: Int) = 42 }

class V3(val x: Int) extends AnyVal { def f(a: Int) = x; def f[T](a: List[T]) = x }
object V3 { def f$extension[T](a: List[T]) = 42 }

// not a clash: an overload with another signature, a method without an extension method,
// and a member of another object.
class V4(val x: Int) extends AnyVal { def f(a: Int) = x; def f(a: String) = x; def h = x }
object V4 { def f$extension(a: Any, b: Any, c: Any) = 42; def g$extension = 42; def x$extension = 42 }
object Other { def h$extension = 42 }

object Test {
  def main(args: Array[String]): Unit = {
    println(new V1(1).f)
    println(new V2(1).f(1))
    println(new V3(1).f(List(1)))
    println(new V4(1).f(1))
  }
}
