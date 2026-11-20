// The body of `G.m` is duplicated into `G$mcI$sp.m$mcI$sp`. `G$mcI$sp` extends `TB$mcI$sp`
// rather than `TB`, so `super[TB]` must be retyped as a super call to that variant.
trait TB[@specialized(Int) T] { def m(t: T): T = t }
class G[@specialized(Int) T] extends TB[T] { override def m(t: T) = super[TB].m(t) }

trait TC[@specialized(Int) T] { def n(t: T): T = t }
class H[@specialized(Int) T] extends TB[T] with TC[T] {
  override def m(t: T) = super[TC].n(t)
  override def n(t: T) = super[TB].m(t)
}

// A `super[TB]` in a local class of a duplicated body refers to the local class's own parent `TB`.
class K[@specialized(Int) T] extends TB[T] {
  def k(t: T) = {
    class Loc extends TB[String] { override def m(x: String) = super[TB].m(x) + "!" }
    new Loc().m(t.toString)
  }
}
