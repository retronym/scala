package lp

class Meter(val v: Double) extends AnyVal with Ordered[Meter] {
  def +(o: Meter): Meter = new Meter(v + o.v)
  def compare(o: Meter): Int = v compare o.v
  def scale[T](t: T)(implicit n: Numeric[T]): Meter = new Meter(v * n.toDouble(t))
  override def toString = s"$v m"
}
class Wrap[A](val a: A) extends AnyVal { def map[B](f: A => B): Wrap[B] = new Wrap(f(a)); private def hidden = a; def viaHidden = hidden }
final case class Tag(s: String) extends AnyVal
object Meter { def zero = new Meter(0); implicit class Rich(val i: Int) extends AnyVal { def m = new Meter(i.toDouble) } }
trait VT[T] { def id(t: T): T; def wrap(t: Wrap[T]): Wrap[T] = t }
class VImpl extends VT[Meter] { def id(t: Meter) = t }
object Values { def arr = Array(new Meter(1)); def opt = Option(new Wrap(1)); def m(x: Meter): Meter = x; def m(x: Int): Int = x }
