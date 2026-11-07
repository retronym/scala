package cc

final case class P7(x: Int, y: String = "d7") { def bump = copy(x = x + 1) }
case class Q7[A](a: A, ps: List[P7]) extends Product with Serializable
sealed trait E7
object E7 {
  final case class L(n: Int) extends E7
  final case class R(s: String, e: E7) extends E7
  case object Z extends E7
  def size(e: E7): Int = e match { case L(_) => 1; case R(_, e1) => 1 + size(e1); case Z => 0 }
}
