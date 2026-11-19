package lp.use23
import lp._

object Use23 {
  def m(s: Shape): String = s match { case Circle(r) if r > 23 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 23 }
  def miss(s: Shape): Int = s match { case _: Circle => 23 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 23 }
  def spec = { val b = new Box(23); val c = b.map(_ * 2); new IntBox(23).get + c.get + new Sum().run(Array(23)) + SpecUse.poly(23).get + Spec.id(23) }
  class Mine extends Box[Long](23.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 23 }
  class M23 extends C2 with Stack1 { override def log(s: String) = super.log(s + 23) ; override lazy val slow = "s23" }
  trait T23 extends WithVal { val mine = tag + 23; def ms = secret + slow.length }
  object O23 extends M23 with T23
  def lam = { var v = 23; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(23) + Lam.localDefs(v) }
  def inner = { val o = new Sub(23); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(23) + Meter.Rich(23).m; val w = new Wrap(23).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 23; def foo(i: Int) = "s" })
}
class Sub23 extends Outer(23) { class X extends In1 { override def f = super.f + 23 }; lazy val x = new X }
case class CC23(a: Int, b: String = "d") extends Shape023 { def area = a * 23 }
sealed trait Shape023
object Dup23 { def apply(i: Int) = CC23(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 23 else go(n - 1) }
