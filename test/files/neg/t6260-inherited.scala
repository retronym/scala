// `IdV.ap` and the bridge for `Id.ap` have the same signature after posterasure, `ap(Object)Object`,
// but `IdV.ap` is called with the unboxed value and the bridge expects a `V`. An anonymous class
// cannot rename its member out of the way of the bridge (as in scala/bug#6260): the bridge would then
// implement `IdV.ap` and throw ClassCastException. A named class is rejected the same way.

class V(val x: Any) extends AnyVal

trait Id[A] { def ap(a: A): A }
trait IdV extends Id[V] { def ap(v: V): V }

object Test {
  val anon: IdV = new IdV { def ap(v: V) = v }
  val sam: IdV = v => v
  class Named extends IdV { def ap(v: V) = v }

  // okay: every overridden method is implemented through the bridge
  val anonId: Id[V] = new Id[V] { def ap(v: V) = v }
  val samId: Id[V] = v => v
}
