package lp.use11
import lp._

object Use11 {
  def m(s: Shape): String = s match { case Circle(r) if r > 11 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 11 }
  def miss(s: Shape): Int = s match { case _: Circle => 11 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 11 }
  def spec = { val b = new Box(11); val c = b.map(_ * 2); new IntBox(11).get + c.get + new Sum().run(Array(11)) + SpecUse.poly(11).get + Spec.id(11) }
  class Mine extends Box[Long](11.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 11 }
  class M11 extends C2 with Stack1 { override def log(s: String) = super.log(s + 11) ; override lazy val slow = "s11" }
  trait T11 extends WithVal { val mine = tag + 11; def ms = secret + slow.length }
  object O11 extends M11 with T11
  def lam = { var v = 11; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(11) + Lam.localDefs(v) }
  def inner = { val o = new Sub(11); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(11) + Meter.Rich(11).m; val w = new Wrap(11).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 11; def foo(i: Int) = "s" })
}
class Sub11 extends Outer(11) { class X extends In1 { override def f = super.f + 11 }; lazy val x = new X }
case class CC11(a: Int, b: String = "d") extends Shape011 { def area = a * 11 }
sealed trait Shape011
object Dup11 { def apply(i: Int) = CC11(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 11 else go(n - 1) }
