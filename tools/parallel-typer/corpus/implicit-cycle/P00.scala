package ic
import Common._
class T0
object P0 {
  import P1._
  implicit val a0 = { implicitly[Base]; new T0 }
  def use0 = a0
}
