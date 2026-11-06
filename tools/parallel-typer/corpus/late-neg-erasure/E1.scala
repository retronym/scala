package lne
class Dbl1 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G1[T] { def g(t: T): Int }
class H1 extends G1[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W1 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
