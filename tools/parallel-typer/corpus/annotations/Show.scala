package ann

import scala.annotation.implicitNotFound

@implicitNotFound("No Show for ${T}: define one in its companion")
trait Show[T] { def show(t: T): String }
object Show {
  implicit val intShow: Show[Int] = _.toString
}
