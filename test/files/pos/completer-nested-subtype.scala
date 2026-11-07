// A completer forced in the middle of a type operation typechecks with its own recursion guards.
// Here, a subtype test past `LogPendingSubTypesThreshold` forces `D1.m`, and the same test in its
// body must not be answered `false` by the pending set.
object Types { type D2 = H { def m: Any } }
import Types._
trait W[+A]
trait H { def m: Any }
object Test {
  def a: W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[D1]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = ???
  def b: W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[W[D2]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = a
}
class D1 extends H { def m = { val z: D2 = (??? : D1); 1 } }
