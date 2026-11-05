package cc

final case class P4(x: Int, y: String = "d4") { def bump = copy(x = x + 1) }
case class Q4[A](a: A, ps: List[P4]) extends Product with Serializable
sealed trait E4
object E4 {
  final case class L(n: Int) extends E4
  final case class R(s: String, e: E4) extends E4
  case object Z extends E4
  def size(e: E4): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
