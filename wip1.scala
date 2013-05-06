trait Test[A]

class Foo {
  def f[A](a: Test[A]): Test[A] = null
  lazy val x: Test[Nothing]     = null
  lazy val x2                   = new Test[Nothing] { }

  f(x)
  f(x2)
  f[Nothing](x)
  f[Nothing](x2)
}
