// Constructor parameters with distinct names that decode to the same string
// (`π` and `$u03C0`, `+` and `$u002B`) are distinct members, also in bytecode.

class C(π: Int, $u03C0: Int) { def s = π * 10 + $u03C0 }
class D(val + : Int, val $u002B: Int)
case class E(`a b`: Int, $u0061$u0020b: Int)
class F(private[this] var π: Int, $u03C0: Int) { def bump() = { π += 100; π + $u03C0 } }

object Test {
  def main(args: Array[String]): Unit = {
    println(new C(1, 2).s)
    val d = new D(3, 4)
    println(s"${d.+} ${d.$u002B}")
    val e = E(5, 6)
    println(s"${e.`a b`} ${e.$u0061$u0020b} $e")
    println(new F(7, 8).bump())
  }
}
