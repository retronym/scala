package ic
import Common._
class T10
object P10 {
  import P11._
  implicit val a10 = { implicitly[Base]; new T10 }
  def use10 = a10
}
