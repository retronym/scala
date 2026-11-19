//> using options -Werror
case class K(q: Int) { private[this] def q(a: Int): Int = a * 2 }
case class G[A](q: A, r: List[A])(s: String) { private[this] def q(a: Int): Int = a; private[this] def r(n: Int): Unit = () }
case class P private (q: Int) { private[this] def q(a: String): String = a }
class Outer { case class I(q: Int) { private[this] def q(a: Int): Int = a } }
