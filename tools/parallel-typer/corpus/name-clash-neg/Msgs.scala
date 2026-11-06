// The messages that TypeDiagnostics disambiguates, by every route, to compare with the serial compiler's.
trait P[-T] { def apply(t: T): Unit }
class Msgs {
  val longs: List[java.lang.Long] = List[Long](1L)
  val same: p.Same = new q.Same
  val gen: p.Gen[p.Same] = new p.Gen[q.Same]
  def tparams[T](x: T) = { def g[T](y: T): T = x; g(x) }
  class Inner; class Outer { class Inner }
  val o = new Outer
  val inner: Inner = new o.Inner
  def m(f: Throwable => Unit) = 1
  def some(c: P[_ >: Throwable]) = m(c.apply)
  def plus[***](x: ***) = { def g[***](y: ***): *** = x; g(x) }
}
