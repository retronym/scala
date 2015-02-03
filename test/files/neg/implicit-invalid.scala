object Test {
  implicit def x: Int = 42
  def foo = implicitly[Int]
  implicit def y = 42
}
