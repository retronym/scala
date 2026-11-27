trait N[-A]
trait Y extends N[Any]
trait D extends N[H.NDY]
// `E <: N[E]` can only be derived from itself
trait E extends N[N[E]]
// neither can `F1 <: N[F1 with F2]`, which goes round through `F2`
trait F1 extends N[N[F1 with F2]]
trait F2 extends N[N[F1 with F2]]
object H {
  type NDY = N[D with Y]
  type NE  = N[E]
  type NF  = N[F1 with F2]
}

// A check that has cut off a cycle and found `D <: H.NDY` on the way still rejects what doesn't conform.
object Test {
  def ok(p: (D, D)): (H.NDY, H.NDY) = p
  def e1(e: E): H.NE = e
  def e2(p: (D, E)): (H.NDY, H.NE) = p
  def e3(p: (E, D)): (H.NE, H.NDY) = p
  def e4(p: (D, E, D)): (H.NDY, H.NE, H.NDY) = p
  def e5(f: H.NDY => E): D => H.NE = f
  def f1(p: (D, F1, D)): (H.NDY, H.NF, H.NDY) = p
  def f2(p: (F2, D, F1)): (H.NF, H.NDY, H.NF) = p
  def d1(p: (D, H.NDY)): (H.NDY, D) = p
  def d2(f: D => D): H.NDY => H.NDY = f
}
