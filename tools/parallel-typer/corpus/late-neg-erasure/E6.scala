package lne
class Dbl6 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G6[T] { def g(t: T): Int }
class H6 extends G6[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W6 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
