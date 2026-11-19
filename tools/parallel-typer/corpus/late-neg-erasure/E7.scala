package lne
class Dbl7 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G7[T] { def g(t: T): Int }
class H7 extends G7[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W7 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
