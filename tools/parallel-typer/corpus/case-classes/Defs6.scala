package cc

final case class P6(x: Int, y: String = "d6") { def bump = copy(x = x + 1) }
case class Q6[A](a: A, ps: List[P6]) extends Product with Serializable
sealed trait E6
object E6 {
  final case class L(n: Int) extends E6
  final case class R(s: String, e: E6) extends E6
  case object Z extends E6
  def size(e: E6): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
