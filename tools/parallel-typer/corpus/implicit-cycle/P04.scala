package ic
import Common._
class T4
object P4 {
  import P5._
  implicit val a4 = { implicitly[Base]; new T4 }
  def use4 = a4
}
