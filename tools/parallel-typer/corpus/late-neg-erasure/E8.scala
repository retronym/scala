package lne
class Dbl8 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G8[T] { def g(t: T): Int }
class H8 extends G8[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W8 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
