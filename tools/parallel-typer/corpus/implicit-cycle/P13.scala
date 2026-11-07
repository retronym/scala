package ic
import Common._
class T13
object P13 {
  import P12._
  implicit val a13 = { implicitly[Base]; new T13 }
  def use13 = a13
}
