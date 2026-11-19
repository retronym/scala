import scala.language.reflectiveCalls

trait Base { def base(i: Int): Int = i }

object Test {
  // The type of `x` abstracts over the member class: `AnyRef { type In <: AnyRef { val v: Int }; def mk(v: Int): this.In; def m(i: this.In): Int; ... }`
  val x = new {
    class In(val v: Int)
    def mk(v: Int) = new In(v)
    def m(i: In): Int = i.v
    def curried(s: String)(i: In): Int = i.v
    def poly[T <: In](t: T): Int = t.v
    def ok(is: List[In]): Int = is.head.v
    def viaM(v: Int): Int = m(mk(v)) // not a reflective call
  }

  def a = x.m(x.mk(1))             // error
  def b = x.curried("")(x.mk(1))   // error
  def c = x.poly(x.mk(1))          // error
  def d = x.m _                    // error
  def e = x.ok(List(x.mk(1)))
  def f = x.viaM(1)
  def g = x.mk(1).v

  // not a structural member, nor a reflective call
  val y = new Base { class In(val v: Int); def mk(v: Int) = new In(v); override def base(i: Int): Int = i + 1; def m(i: In): Int = i.v }
  def h = y.base(1)
  def i = y.m(y.mk(1))             // error
}
