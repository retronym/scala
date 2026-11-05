package cc

final case class P1(x: Int, y: String = "d1") { def bump = copy(x = x + 1) }
case class Q1[A](a: A, ps: List[P1]) extends Product with Serializable
sealed trait E1
object E1 {
  final case class L(n: Int) extends E1
  final case class R(s: String, e: E1) extends E1
  case object Z extends E1
  def size(e: E1): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
