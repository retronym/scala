class C(val b: B) extends AnyVal
class B(val i: Int) extends AnyVal

object Test {
  def foo(c: C): Int = {
    new C(new B(c.b.i))
    c.b.i
  }
  def bar: Int = foo(new C(new B(42)))
  def main(args: Array[String]): Unit = {
    bar
  }

}
