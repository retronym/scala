package cc

final case class P2(x: Int, y: String = "d2") { def bump = copy(x = x + 1) }
case class Q2[A](a: A, ps: List[P2]) extends Product with Serializable
sealed trait E2
object E2 {
  final case class L(n: Int) extends E2
  final case class R(s: String, e: E2) extends E2
  case object Z extends E2
  def size(e: E2): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
