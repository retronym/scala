//> using options -Werror -Wconf:cat=unchecked:s

// A bounded wildcard in a type pattern is parsed as an existential type whose
// quantifiers must be visible when typing the pattern.
class C[A]

object Test {
  def a(x: Any) = x match { case _: List[_ <: Int] => 1; case _ => 2 }
  def b(x: Any) = x match { case _: Array[_ <: Int] => 1; case _ => 2 }
  def c(x: Any) = x match { case _: Map[_ <: Int, _ >: String] => 1; case _ => 2 }
  def d(x: Any) = x match { case y: List[_ <: Int] => y.headOption; case _ => None }
  def e(x: Any) = x match { case _: (List[T] forSome { type T <: Int }) => 1; case _ => 2 }
  def f(x: Any) = x match { case _: Option[List[_ >: Null <: String]] => 1; case _ => 2 }
  def g(x: C[_ <: AnyRef]) = x match { case _: C[_ <: String] => 1; case _ => 2 }
  def h(xs: List[Any]) = xs match { case (y: List[_ <: Int]) :: _ => y.headOption; case _ => None }
  def i(xs: Seq[Any]) = xs match { case _: Seq[_ <: Int] => 1; case _ => 2 }
  def j(x: Any) = x match { case _: List[_ <: Int] | _: Set[_ <: Int] => 1; case _ => 2 }
}
