package lne
class Dbl3 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G3[T] { def g(t: T): Int }
class H3 extends G3[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W3 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
