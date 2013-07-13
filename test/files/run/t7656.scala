object Test extends App {
  val u = reflect.runtime.universe
  import u._

  def foo(a: Int, b: String) = (a, b)
  val s = ""

  println(u.reify { foo(b = s, a = -0.toInt) })
  println(foo(b = "1", a = 0.toInt))
  println(u.reify { foo(b = Test.this.toString, a = 0) })
}
