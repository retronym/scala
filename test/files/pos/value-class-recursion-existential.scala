class V[T](val t: T) extends AnyVal {
  def f(o: V[_], n: Int): Int = if (n == 0) 0 else o.f(o, n - 1)
}

class W[A, B](val a: A) extends AnyVal {
  def f(o: W[_, _], n: Int): Int = if (n == 0) 0 else o.f(o, n - 1)
}
