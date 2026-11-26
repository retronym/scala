trait Foo[@specialized(Int) P] { def foo: P }
trait FooPlus[@specialized(Int) P] extends Foo[P] { override def foo: P }
class FooImpl extends Foo[Int] { def foo = 1 }
class FooPlusImpl extends FooImpl with FooPlus[Int]

object Test {
  def main(args: Array[String]): Unit = {
    val v: Foo[Int] = new FooPlusImpl
    println(v.foo)
    println(new FooPlusImpl().foo)
  }
}
