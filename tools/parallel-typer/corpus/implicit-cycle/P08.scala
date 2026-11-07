package ic
import Common._
class T8
object P8 {
  import P9._
  implicit val a8 = { implicitly[Base]; new T8 }
  def use8 = a8
}
