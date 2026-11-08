package ic
import Common._
class T7
object P7 {
  import P6._
  implicit val a7 = { implicitly[Base]; new T7 }
  def use7 = a7
}
