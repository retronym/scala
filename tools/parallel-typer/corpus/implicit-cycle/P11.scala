package ic
import Common._
class T11
object P11 {
  import P10._
  implicit val a11 = { implicitly[Base]; new T11 }
  def use11 = a11
}
