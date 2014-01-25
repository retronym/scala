trait Thing { type A }
object IntThing extends Thing { type A = Int }
 
// compiles
package p1 {
  class View(val in: Thing { type A = Int }) {          def f(p: in.A): in.A = p }
  class SubView extends View(IntThing)       { override def f(p: in.A): in.A = p }
}
// does not compile
package p2 {
  class View[AIn](val in: Thing { type A = AIn }) {          def f(p: in.A): in.A = p }
  class SubView extends View[Int](IntThing)       { override def f(p: in.A): in.A = p }
}
