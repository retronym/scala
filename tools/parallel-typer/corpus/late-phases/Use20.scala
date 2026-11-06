package lp.use20
import lp._

object Use20 {
  def m(s: Shape): String = s match { case Circle(r) if r > 20 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 20 }
  def miss(s: Shape): Int = s match { case _: Circle => 20 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 20 }
  def spec = { val b = new Box(20); val c = b.map(_ * 2); new IntBox(20).get + c.get + new Sum().run(Array(20)) + SpecUse.poly(20).get + Spec.id(20) }
  class Mine extends Box[Long](20.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 20 }
  class M20 extends C2 with Stack1 { override def log(s: String) = super.log(s + 20) ; override lazy val slow = "s20" }
  trait T20 extends WithVal { val mine = tag + 20; def ms = secret + slow.length }
  object O20 extends M20 with T20
  def lam = { var v = 20; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(20) + Lam.localDefs(v) }
  def inner = { val o = new Sub(20); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(20) + Meter.Rich(20).m; val w = new Wrap(20).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 20; def foo(i: Int) = "s" })
}
class Sub20 extends Outer(20) { class X extends In1 { override def f = super.f + 20 }; lazy val x = new X }
case class CC20(a: Int, b: String = "d") extends Shape020 { def area = a * 20 }
sealed trait Shape020
object Dup20 { def apply(i: Int) = CC20(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 20 else go(n - 1) }
