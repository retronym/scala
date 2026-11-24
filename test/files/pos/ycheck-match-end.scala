//> using options -Ycheck:_ -Werror

// The parameter of the `matchEnd` label has no owner, and the checker reported a reference
// to `A` in its type as out of scope after patmat.
object Test {
  def first[A](xs: List[A]): Option[A] = xs match {
    case x :: _ => Some(x)
    case _      => None
  }
  def pick[A](a: A, b: A, c: Int): A = c match {
    case 0 => a
    case _ => b
  }
  // uncurry moves the label into a function
  def by[T](t: => T): T = t
  def inByName[A](xs: List[A]): Option[A] = by(xs match { case x :: _ => Some(x); case _ => None })
  def inLambda[A](xs: List[List[A]]): List[Option[A]] = xs.map { case x :: _ => Some(x); case _ => None }
}
