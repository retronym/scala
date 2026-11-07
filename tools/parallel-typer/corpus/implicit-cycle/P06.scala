package ic
import Common._
class T6
object P6 {
  import P7._
  implicit val a6 = { implicitly[Base]; new T6 }
  def use6 = a6
}
