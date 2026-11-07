package ic
import Common._
class T12
object P12 {
  import P13._
  implicit val a12 = { implicitly[Base]; new T12 }
  def use12 = a12
}
