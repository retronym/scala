class A {
  def Foo(x: Int): AnyRef = "A.Foo"
  def call = Foo(1)
}
class B extends A {
  implicit class Foo(val x: Int)            // error, `new B().call` would return the wrapper
}

trait T1 { def Bar(x: Int): AnyRef = "T1.Bar" }
trait T2 { implicit class Bar(val x: Int) }
class C extends T1 with T2                  // error

trait Abs { def Baz(x: Int): AnyRef }
class D extends Abs {
  implicit class Baz(val x: Int)            // ok, implements the deferred member
}

class E extends A {
  implicit class Foo(val x: String)         // ok, an overload
}

// synthetic members that may override without a modifier in source
class P { def f(x: Int = 1): Int = x }
class Q extends P { override def f(x: Int = 2): Int = x }

class Companion { def apply(x: Int): K = null; def unapply(k: K): Option[Int] = None }
object K extends Companion
case class K(x: Int)

trait Prod { def productArity = 5; def canEqual(that: Any) = false; def productPrefix = "p"; def productElement(i: Int): Any = 1 }
case class L(x: Int) extends Prod
