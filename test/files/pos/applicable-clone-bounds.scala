// Overload resolution must not rewrite the bounds of a polymorphic alternative's type params.
// Each `use1` resolves an overload whose type param `B` stays undetermined while `A` is solved;
// inference then substitutes `A`'s solution into `B`'s bound. If that happened on `g`'s own
// type params, `use2` would see the rewritten bound and fail.
object Upper {
  def g[A, B <: List[A]](a: A): Set[B] = ???
  def g(s: Boolean): Int = 0
  def use1 = g(1)
  def use2 = g[String, List[String]]("x")
}

object ForwardReference {
  def g[B <: List[A], A](a: A): Set[B] = ???
  def g(s: Boolean): Int = 0
  def use1 = g(1)
  def use2 = g[List[String], String]("x")
}

object HigherKinded {
  def g[A, F[X <: A]](a: A): Set[F[A]] = ???
  def g(s: Boolean): Int = 0
  type Id[X] = X
  def use1 = g(1)
  def use2 = g[String, Id]("x")
}

object Refinement {
  trait HasT { type T }
  def g[A, B <: HasT { type T = A }](a: A): Set[B] = ???
  def g(s: Boolean): Int = 0
  def use1 = g(1)
  def use2 = g[String, HasT { type T = String }]("x")
}

object Existential {
  def g[A, B <: List[_ <: A]](a: A): Set[B] = ???
  def g(s: Boolean): Int = 0
  def use1 = g(1)
  def use2 = g[String, List[String]]("x")
}

object TwoArgs {
  def g[A, B <: List[A]](a: A, b: Int): Set[B] = ???
  def g[A](a: A, b: String): Int = 0
  def use1 = g(1, 1)
  def use2 = g[String, List[String]]("x", 1)
}
