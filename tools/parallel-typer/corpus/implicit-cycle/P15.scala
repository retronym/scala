package ic
import Common._
class T15
object P15 {
  import P14._
  implicit val a15 = { implicitly[Base]; new T15 }
  def use15 = a15
}
