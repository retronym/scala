//> using options -Werror -Xlint:deprecation
//
class A {
  class X { def hi = "A#X" }
  trait Y
  class Z
  def mk: X = new X
}

class B extends A { type X = Int }             // warn
class C extends A { override type X = Int }    // warn
class D extends A { type Y = String }          // warn

trait T { type Z = Int }
class E extends A with T                       // warn
trait U { class W }
trait V { type W = Int }
class UV extends U with V                      // warn
class VU extends V with U                      // error
class F extends T { class Z }                  // error

abstract class G extends A { type X }          // ok, the abstract type is implemented by the inherited class
abstract class H extends A { type X <: Int }   // error, the class is not within the bounds
class E2 extends E                             // ok, warned in E
class E3 extends E with T                      // ok, as above

// not overridden: the inherited class is not accessible
class M extends java.util.HashMap[Int, Int] { type Values = Int; type KeySet = String }
class P { private class Q; private[this] class Q1 }
class R extends P { type Q = Int; type Q1 = Int }
package p1 { class P2 { private[p1] class Q2 } }
package p2 { class R2 extends p1.P2 { type Q2 = Int } }
