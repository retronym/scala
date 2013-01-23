package t6259

import scala.reflect.runtime.universe._

class A[X](implicit val tt: TypeTag[X]) {println(tt)}

object Obj {
  val x = {
    object InVal extends A[String]
    InVal
    5
  }
}

object Test extends App {
  Obj
}
