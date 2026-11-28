object Test {
  def f[A, B >: List[A]](a: A, b: B): B = b
  def opt[A, B >: Option[A]](a: A): B = Some(a)

  val one = 1
  f(one, Nil)
  // A is widened to Int; the copy of A's solution in B's bound must be widened with it
  f(1, Nil)
  f("s", null)
  opt(true)
}
