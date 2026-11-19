// A top-level glb of refinements that forces a lazy type completer can be re-entered by a
// glb in the completed code. The nested glb must not inherit the outer glb's count towards
// `globalGlbLimit`, otherwise it drops the members of the refinements and the inferred type
// depends on typechecking order.
//> using options -language:reflectiveCalls
object Test { def g(b: Boolean) = if (b) ((a: A { def x: Any }) => ()) else ((a: Q { def y: Int }) => ()) } // glb(A{x}, Q{y}) forces Q.x
trait A
trait Q {
  def y: Int
  def x = if (true) ((a: AnyRef { def m: Int }) => a.m) else ((a: AnyRef { def n: Int }) => a.n)
}
object Check {
  def same[T](t: T)(implicit ev: T =:= (AnyRef { def m: Int; def n: Int } => Int)) = ()
  def q: Q = null
  same(q.x)
}
