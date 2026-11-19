// SLS 6.10: in `val x: S = new C`, the concrete self type of `new C` is `C with x.type`.
object O {
  class C { this: O.x.type => }
  val x: C = new C

  class E { this: O.lz.type => }
  lazy val lz: E = new E

  class F(val i: Int) { this: O.f.type => }
  val f: F = new F(1)
}

class K {
  class C { this: K.this.x.type => }
  final val x: C = new C
  private[this] class D { this: K.this.y.type => }
  private[this] val y: D = new D
}

object L {
  class D { this: D with Singleton => }
  val d = new D
  def m = { val y = new D; y }
  def n: D = { val y: D = new D; y }
}
