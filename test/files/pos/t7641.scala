object Test {
  trait TC[_]
  implicit val tcInt: TC[Int] = ???
  def bar[T](t:T)(implicit x: TC[T]): Int = 0
  def foo[B](f: Int => B) {}
  foo (x => bar(x)) // okay
  foo (bar) // fails
}
