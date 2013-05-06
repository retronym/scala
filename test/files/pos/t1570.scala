class A {
  trait Test[A]
  trait Test2[+A]

  def f[A](a: Test[A]): Test[A]   = sys.error("")
  def f2[A](a: Test[A]): Test2[A] = sys.error("")
  lazy val x: Test[Nothing]       = sys.error("")

  f(x)
  f[Nothing](x)
  f[scala.Nothing](x)

  f2(x)
  f2[Nothing](x)
  f2[scala.Nothing](x)
}
