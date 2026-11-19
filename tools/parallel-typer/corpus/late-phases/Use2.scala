package lp.use2
import lp._

object Use2 {
  def m(s: Shape): String = s match { case Circle(r) if r > 2 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 2 }
  def miss(s: Shape): Int = s match { case _: Circle => 2 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 2 }
  def spec = { val b = new Box(2); val c = b.map(_ * 2); new IntBox(2).get + c.get + new Sum().run(Array(2)) + SpecUse.poly(2).get + Spec.id(2) }
  class Mine extends Box[Long](2.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 2 }
  class M2 extends C2 with Stack1 { override def log(s: String) = super.log(s + 2) ; override lazy val slow = "s2" }
  trait T2 extends WithVal { val mine = tag + 2; def ms = secret + slow.length }
  object O2 extends M2 with T2
  def lam = { var v = 2; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(2) + Lam.localDefs(v) }
  def inner = { val o = new Sub(2); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(2) + Meter.Rich(2).m; val w = new Wrap(2).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 2; def foo(i: Int) = "s" })
}
class Sub2 extends Outer(2) { class X extends In1 { override def f = super.f + 2 }; lazy val x = new X }
case class CC2(a: Int, b: String = "d") extends Shape02 { def area = a * 2 }
sealed trait Shape02
object Dup2 { def apply(i: Int) = CC2(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 2 else go(n - 1) }
