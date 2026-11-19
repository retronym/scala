package cc

final case class P8(x: Int, y: String = "d8") { def bump = copy(x = x + 1) }
case class Q8[A](a: A, ps: List[P8]) extends Product with Serializable
sealed trait E8
object E8 {
  final case class L(n: Int) extends E8
  final case class R(s: String, e: E8) extends E8
  case object Z extends E8
  def size(e: E8): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
