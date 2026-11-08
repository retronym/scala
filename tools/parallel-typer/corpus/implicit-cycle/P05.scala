package ic
import Common._
class T5
object P5 {
  import P4._
  implicit val a5 = { implicitly[Base]; new T5 }
  def use5 = a5
}
