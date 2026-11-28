import scala.annotation.tailrec

class V[T](val t: T) extends AnyVal {
  // The recursive call is not redirected to the extension method, as the qualifier
  // doesn't supply the type argument for `T`.
  @tailrec final def f(o: V[_], n: Int): Int = if (n == 0) 0 else o.f(o, n - 1)
  @tailrec final def g(o: V[String], n: Int): Int = if (n == 0) 0 else o.g(o, n - 1)
}
