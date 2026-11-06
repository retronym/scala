package lne
class Dbl2 { def a(x: List[Int]): Int = 1; def a(x: List[String]): Int = 2 }
trait G2[T] { def g(t: T): Int }
class H2 extends G2[String] { def g(t: String) = 1; def g(t: Object) = 2 }
object W2 { def s(x: Any) = x match { case _: List[Int] => 1; case _ => 2 } }
