class A {
  class X { def hi = "A#X" }
  trait Y
  class Z
  def mk: X = new X
}

class B extends A { type X = Int }             // error
class C extends A { override type X = Int }    // error
class D extends A { type Y = String }          // error

trait T { type Z = Int }
class E extends A with T                       // error
trait U { class W }
trait V { type W = Int }
class UV extends U with V                      // error
class VU extends V with U                      // error
class F extends T { class Z }                  // error

abstract class G extends A { type X }          // ok, the abstract type is implemented by the inherited class
abstract class H extends A { type X <: Int }   // error, the class is not within the bounds
