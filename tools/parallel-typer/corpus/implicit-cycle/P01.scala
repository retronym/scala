package ic
import Common._
class T1
object P1 {
  import P0._
  implicit val a1 = { implicitly[Base]; new T1 }
  def use1 = a1
}
