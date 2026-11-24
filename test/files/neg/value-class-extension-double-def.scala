class V(val x: Int) extends AnyVal { def f(a: Int) = x }
object V { def f$extension(v: V, a: Int) = 42 }

object Test {
  def main(args: Array[String]): Unit = println(new V(1).f(1))
}
