package lp.use4
import lp._

object Use4 {
  def m(s: Shape): String = s match { case Circle(r) if r > 4 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 4 }
  def miss(s: Shape): Int = s match { case _: Circle => 4 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 4 }
  def spec = { val b = new Box(4); val c = b.map(_ * 2); new IntBox(4).get + c.get + new Sum().run(Array(4)) + SpecUse.poly(4).get + Spec.id(4) }
  class Mine extends Box[Long](4.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 4 }
  class M4 extends C2 with Stack1 { override def log(s: String) = super.log(s + 4) ; override lazy val slow = "s4" }
  trait T4 extends WithVal { val mine = tag + 4; def ms = secret + slow.length }
  object O4 extends M4 with T4
  def lam = { var v = 4; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(4) + Lam.localDefs(v) }
  def inner = { val o = new Sub(4); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(4) + Meter.Rich(4).m; val w = new Wrap(4).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 4; def foo(i: Int) = "s" })
}
class Sub4 extends Outer(4) { class X extends In1 { override def f = super.f + 4 }; lazy val x = new X }
case class CC4(a: Int, b: String = "d") extends Shape04 { def area = a * 4 }
sealed trait Shape04
object Dup4 { def apply(i: Int) = CC4(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 4 else go(n - 1) }
