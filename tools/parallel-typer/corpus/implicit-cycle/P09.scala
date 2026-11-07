package ic
import Common._
class T9
object P9 {
  import P8._
  implicit val a9 = { implicitly[Base]; new T9 }
  def use9 = a9
}
