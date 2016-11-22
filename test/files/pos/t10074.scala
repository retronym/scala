object A {
  f[Int](x => 1)
  def f[T](f: Any => T) = ()
  def f[T](f: Any => T, v: Int) = ()
}
