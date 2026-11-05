package cc

final case class P5(x: Int, y: String = "d5") { def bump = copy(x = x + 1) }
case class Q5[A](a: A, ps: List[P5]) extends Product with Serializable
sealed trait E5
object E5 {
  final case class L(n: Int) extends E5
  final case class R(s: String, e: E5) extends E5
  case object Z extends E5
  def size(e: E5): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
