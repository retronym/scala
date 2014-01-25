trait CC[A]
trait HasElem[Repr] { type A }
object ccInt extends HasElem[CC[Int]] { type A = Int }
 
object Test {
  final class View[Repr, AIn](repr: Repr, val tc: HasElem[Repr] { type A = AIn }) {
    // For a time it's completely convinced AIn and tc.A are the same type.
    def equivalence1[B](implicit ev: B =:= AIn) = true
    def equivalence2[B](implicit ev: B =:= tc.A) = true
    def evidences = List(equivalence1[tc.A], equivalence1[AIn], equivalence2[tc.A], equivalence2[AIn]) // 4 out of 4
 
    // Foreshadow.
    def f1(p: AIn): AIn = p
    def f2(p: tc.A): tc.A = p
  }
 
  def xs: CC[Int] = ???
  val view = new View[CC[Int], Int](xs, ccInt)
 
  // No longer equivalent, transitivity lost.
  def g0 = List(view.equivalence1[Int], view.equivalence2[Int])
  // b.scala:33: error: Cannot prove that Int =:= Test.view.tc.A.
  //   def g0 = List(view.equivalence1[Int], view.equivalence2[Int])
  //                                                          ^
 
  def g1 = view f1 5  // compiles
  def g2 = view f2 5  // fails
  // b.scala:31: error: type mismatch;
  //  found   : Int(5)
  //  required: Test.view.tc.A
  //     (which expands to)  AIn
  //   def g2 = view f2 5  // fails
  //                    ^
}
