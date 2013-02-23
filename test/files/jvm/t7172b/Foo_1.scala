class Foo_1 {
  def foo(x: AnyRef) {
    println("".isInstanceOf[AnyRef]) // statically true (modulo null)

    class A
    final class B
    class C extends A
    println((new A()).isInstanceOf[B]) // statically false (modulo null)
    println((new B()).isInstanceOf[A]) // statically false (modulo null)
    println((new C()).isInstanceOf[A]) // statically true (modulo null)
  }
}
