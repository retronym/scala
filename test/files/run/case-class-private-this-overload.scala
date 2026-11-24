// A `private[this]` method that overloads a case class parameter name
// must not be taken for the parameter's accessor by the synthetic members.
case class K(q: Int) { private[this] def q(a: Int): Int = a * 2; def dbl = q(q) }
case class L(q: Int, r: String) { private[this] def r(a: Int): String = "x" * a; def rr = r(q) }
case class V(q: Int*) { private[this] def q(a: Int): Int = a }

object Test {
  def main(args: Array[String]): Unit = {
    val k = K(3)
    println(k)
    println(k.dbl)
    println(K.unapply(k))
    println(k match { case K(n) => n })
    println(k.copy(q = 4))
    println(k == K(3))
    println(k.hashCode == K(3).hashCode)
    println(k.productElement(0))
    val l = L(2, "s")
    println(l)
    println(l.rr)
    println(L.unapply(l))
    println(l match { case L(a, b) => (a, b) })
    println(l.copy(r = "t"))
    val v = V(1, 2)
    println(V.unapplySeq(v))
    println(v match { case V(a, b) => a + b case _ => -1 })
  }
}
