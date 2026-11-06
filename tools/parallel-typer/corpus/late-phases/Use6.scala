package lp.use6
import lp._

object Use6 {
  def m(s: Shape): String = s match { case Circle(r) if r > 6 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 6 }
  def miss(s: Shape): Int = s match { case _: Circle => 6 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 6 }
  def spec = { val b = new Box(6); val c = b.map(_ * 2); new IntBox(6).get + c.get + new Sum().run(Array(6)) + SpecUse.poly(6).get + Spec.id(6) }
  class Mine extends Box[Long](6.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 6 }
  class M6 extends C2 with Stack1 { override def log(s: String) = super.log(s + 6) ; override lazy val slow = "s6" }
  trait T6 extends WithVal { val mine = tag + 6; def ms = secret + slow.length }
  object O6 extends M6 with T6
  def lam = { var v = 6; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(6) + Lam.localDefs(v) }
  def inner = { val o = new Sub(6); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(6) + Meter.Rich(6).m; val w = new Wrap(6).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 6; def foo(i: Int) = "s" })
}
class Sub6 extends Outer(6) { class X extends In1 { override def f = super.f + 6 }; lazy val x = new X }
case class CC6(a: Int, b: String = "d") extends Shape06 { def area = a * 6 }
sealed trait Shape06
object Dup6 { def apply(i: Int) = CC6(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 6 else go(n - 1) }
