package scala.reflect.internal.util

final class OneElemCache[T >: Null <: AnyRef](create: () => T) {
  var elem: T = create()
  @inline final def use[A](f: T => A): A = {
    val e = elem
    if (e != null) {
      elem = null
      try f(e)
      finally elem = e
    } else {
      f(create())
    }
  }
}
