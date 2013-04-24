trait Base {
  def foo(implicit a: Any) = 0
}

trait Derived1 extends Base {
  def foo = 0
}

trait Derived2 extends Base {
  val foo = 0
}
