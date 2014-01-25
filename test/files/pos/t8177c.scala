trait HasElem { type A }
trait View[AIn] {
  trait T extends HasElem { type A = AIn }
  val tc: T //HasElem { type A = AIn }
  def f2(p: tc.A): tc.A = p
}

object Test {
  val view: View[Int] = null
 
  view f2 5  // fails
}
