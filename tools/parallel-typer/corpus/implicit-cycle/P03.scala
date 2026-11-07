package ic
import Common._
class T3
object P3 {
  import P2._
  implicit val a3 = { implicitly[Base]; new T3 }
  def use3 = a3
}
