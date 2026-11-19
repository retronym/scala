package pt

// superaccessors expands the name of a member that is private[C] for a class C; `y`'s type names `x`,
// so a subclass in another file refers to `x` from its own pickle
class Qualified {
  private[Qualified] val x: Foo = new Foo
  val y: x.T = x.t
  private[Qualified] def hidden(i: Int): Int = i + 1
  def shown(i: Int): Int = hidden(i)
  class Inner {
    private[Qualified] var state: Int = 0
    def touch(): Int = { state += 1; state }
  }
}

object Qualified {
  private[Qualified] val inCompanion: Int = 1
  def read(q: Qualified): String = q.x.t + inCompanion
}

// a private member selected inside an @inline method is made not private, with an expanded name
class Inlined {
  private var count: Int = 0
  private def step: Int = 2
  @inline final def inc(): Int = { count += step; count }
}
