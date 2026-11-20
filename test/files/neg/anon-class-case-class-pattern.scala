// A case class of an anonymous class, matched from outside through the inferred type of the anonymous class,
// crashed in explicitouter: "Internal error: unable to find the outer accessor symbol of object Test".
object Test {
  val y = new { case class P(a: Int); def inside(x: Any) = x match { case P(a) => a; case _ => -1 } }
  def f(x: Any) = x match { case y.P(a) => a; case _ => -1 } // error
}
