package cc

final case class P3(x: Int, y: String = "d3") { def bump = copy(x = x + 1) }
case class Q3[A](a: A, ps: List[P3]) extends Product with Serializable
sealed trait E3
object E3 {
  final case class L(n: Int) extends E3
  final case class R(s: String, e: E3) extends E3
  case object Z extends E3
  def size(e: E3): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
