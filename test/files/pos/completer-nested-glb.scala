// A completer forced in the middle of a type operation typechecks with its own recursion guards.
// Here, the refinement step of a glb forces `A3.m`, and the glb in its body must keep its members.
trait P; trait B3; trait A1; trait B1
object Lib {
  def pick[T](f: T => Unit, g: T => Unit): T = ???
  def fa: ((P { def x: Int }) => Unit) = ???
  def fb: ((P { def y: Int }) => Unit) = ???
}
object Test {
  def f1: ((A1 { def k: A3 }) => Unit) = ???
  def f2: ((B1 { def k: B3 { def m: Any } }) => Unit) = ???
  def forced = Lib.pick(f1, f2)
}
trait A3 extends B3 { def m = Lib.pick(Lib.fa, Lib.fb) }
object Use { def v(a: A3) = a.m.x + a.m.y }
