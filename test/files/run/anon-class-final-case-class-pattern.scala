// A final case class of an anonymous class, matched from outside through the inferred type of the anonymous
// class. Its outer test is dropped with a warning, so the pattern works, unlike that of a non-final case class
// (neg/anon-class-case-class-pattern.scala).
object Test {
  val y = new { final case class P(a: Int) }
  def f(x: Any) = x match { case y.P(a) => a; case _ => -1 }

  def local(n: Int) = {
    val z = new { final case class P(a: Int) }
    def g(x: Any) = x match { case z.P(a) => a; case _ => -1 }
    (g(z.P(n)), g(n))
  }

  def main(args: Array[String]): Unit = {
    assert(f(y.P(3)) == 3)
    assert(f(3) == -1)
    assert(local(4) == ((4, -1)))
  }
}
