// A generic case class of an anonymous class that extends a trait with an abstract type member:
// after the errors below, typing the case body `a` overflowed the stack in `AsSeenFromMap`.
object Test {
  def f = {
    val x = new { trait TM { type E; def e: E }; case class P[A](a: A) extends TM { type E = A; def e = a } } // error
    val t: x.TM = x.P(1) // error
    t match { case x.P(a) => a } // error
  }
}
