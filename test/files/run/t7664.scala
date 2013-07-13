import reflect.runtime.universe._

object Test extends App {
  class C { def foo(a: Any)(b: Any) = a }
  def c = new C

  println(reify{def x(c: => C) = c.foo(c) _}.tree)

  // We want this to lift out the references to `c` before constructing
  // the closure.
  println(reify{def byname(f: (=> C) => Any) = (); byname(c => c.foo(c) _)}.tree)
}
