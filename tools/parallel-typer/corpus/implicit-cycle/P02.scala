package ic
import Common._
class T2
object P2 {
  import P3._
  implicit val a2 = { implicitly[Base]; new T2 }
  def use2 = a2
}
