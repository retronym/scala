package lp.use12
import lp._

object Use12 {
  def m(s: Shape): String = s match { case Circle(r) if r > 12 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 12 }
  def miss(s: Shape): Int = s match { case _: Circle => 12 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 12 }
  def spec = { val b = new Box(12); val c = b.map(_ * 2); new IntBox(12).get + c.get + new Sum().run(Array(12)) + SpecUse.poly(12).get + Spec.id(12) }
  class Mine extends Box[Long](12.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 12 }
  class M12 extends C2 with Stack1 { override def log(s: String) = super.log(s + 12) ; override lazy val slow = "s12" }
  trait T12 extends WithVal { val mine = tag + 12; def ms = secret + slow.length }
  object O12 extends M12 with T12
  def lam = { var v = 12; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(12) + Lam.localDefs(v) }
  def inner = { val o = new Sub(12); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(12) + Meter.Rich(12).m; val w = new Wrap(12).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 12; def foo(i: Int) = "s" })
}
class Sub12 extends Outer(12) { class X extends In1 { override def f = super.f + 12 }; lazy val x = new X }
case class CC12(a: Int, b: String = "d") extends Shape012 { def area = a * 12 }
sealed trait Shape012
object Dup12 { def apply(i: Int) = CC12(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 12 else go(n - 1) }
