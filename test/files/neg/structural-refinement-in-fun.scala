import scala.language.reflectiveCalls

class VC(val i: Int) extends AnyVal
class P { def vc(v: VC) = v.i; def rvc: VC = new VC(1) }

object Test {
  // The refinements are checked even when typed as the qualifier of an application,
  // which the typer first attempts silently.
  def f = (new P: { def vc(v: VC): Int }).vc(new VC(3))
  def g = (new P: { def rvc: VC }).rvc
  def h(p: P) = p.asInstanceOf[{ def vc(v: VC): Int }].vc(new VC(3))
  def k[T](p: P, t: T) = p.asInstanceOf[{ def m(t: T): Int }].m(t)
}
