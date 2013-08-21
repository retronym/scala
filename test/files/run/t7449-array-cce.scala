class ValA[A](val a: A) extends AnyVal

class C { def foo[Z](as: ValA[Array[Int]]) = as.a.head }

object Test extends App {
  val x = new C().foo(new ValA(new Array[Int](1)))
  assert(x == 1)
}
