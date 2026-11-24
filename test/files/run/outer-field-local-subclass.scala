//> using options -Ycheck:erasure,constructors

import scala.language.reflectiveCalls

// Local and anonymous subclasses of an inner class reach the outer instance through the
// `$outer` field of their parent when it is the same instance.
class O(val name: String) {
  class B { def outerName = name }
  def f = new B { def g = O.this }
  def h = { class L extends B { def g = O.this }; new L }
  def unused = new B { }
}

trait T { val name: String; class B { def outerName = name }; def f = new B { def g = T.this } }
object TO extends T { val name = "TO" }

object Test {
  def main(args: Array[String]): Unit = {
    val o = new O("o")
    println((o.f.g.name, o.f.outerName))
    println((o.h.g.name, o.h.outerName))
    println(o.unused.outerName)
    println((TO.f.g.name, TO.f.outerName))
  }
}
