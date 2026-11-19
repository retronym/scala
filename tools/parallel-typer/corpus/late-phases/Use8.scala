package lp.use8
import lp._

object Use8 {
  def m(s: Shape): String = s match { case Circle(r) if r > 8 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 8 }
  def miss(s: Shape): Int = s match { case _: Circle => 8 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 8 }
  def spec = { val b = new Box(8); val c = b.map(_ * 2); new IntBox(8).get + c.get + new Sum().run(Array(8)) + SpecUse.poly(8).get + Spec.id(8) }
  class Mine extends Box[Long](8.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 8 }
  class M8 extends C2 with Stack1 { override def log(s: String) = super.log(s + 8) ; override lazy val slow = "s8" }
  trait T8 extends WithVal { val mine = tag + 8; def ms = secret + slow.length }
  object O8 extends M8 with T8
  def lam = { var v = 8; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(8) + Lam.localDefs(v) }
  def inner = { val o = new Sub(8); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(8) + Meter.Rich(8).m; val w = new Wrap(8).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 8; def foo(i: Int) = "s" })
}
class Sub8 extends Outer(8) { class X extends In1 { override def f = super.f + 8 }; lazy val x = new X }
case class CC8(a: Int, b: String = "d") extends Shape08 { def area = a * 8 }
sealed trait Shape08
object Dup8 { def apply(i: Int) = CC8(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 8 else go(n - 1) }
