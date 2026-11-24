// A trait local to a constructor (here: to a super-constructor argument or an early
// definition) has the outer class of the enclosing class as its outer class. The outer
// accessor implemented for it in a subclass must return that instance.

class P(val a: Any)

class Outer(val name: String) {
  def who = name

  class I1 extends P({ trait LT { def kk = who }; class LC extends LT; new LC().kk })

  class I2 extends P({ trait LT { def kk = who }; new LT {}.kk })

  class I3 extends { val e = { trait LT { def kk = who }; class LC extends LT; new LC().kk } } with P(null)

  class I4 extends P({ trait LT { def kk = 1 }; class LC extends LT; new LC().kk })
}

object Test {
  def main(args: Array[String]): Unit = {
    val o = new Outer("o")
    println(new o.I1().a)
    println(new o.I2().a)
    println(new o.I3().e)
    println(new o.I4().a)
  }
}
