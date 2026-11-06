package lp

class Outer(val a: Int) { outer =>
  private val secret = a + 1
  private def priv(x: Int) = x + secret
  class In1 { val b = secret; def f = priv(b); class In2 { def g = f + a + outer.a; object Deep { def h = g } } }
  object InO { val c = a; def mk = new In1 }
  def local = { class L { def v = a }; new L().v }
  def anon = new Runnable { def run(): Unit = println(secret) }
  protected[lp] class Pr
  private[this] class Pt { def x = 1 }
  def pt = new Pt().x
}
class Sub(a: Int) extends Outer(a) { class In3 extends In1 { override def f = super.f + 1 }; val o = new In3 }
object Top { class SI { def t = 1 }; object Nested { class NN { def t = new SI().t }; object Most { val v = new NN } } }
class Ctor(x: Int, y: => Int, private val z: Int)(implicit val q: Ordering[Int]) { val w = x + 1; def l = () => w + z; if (x > 0) println(y) ; var mutable = 0; lazy val lz = z + w }
class TP[T](val t: T) { def cls(implicit ct: scala.reflect.ClassTag[T]) = ct; class Dep { def d: T = t }; val dep = new Dep; val other: TP[T]#Dep = dep }
