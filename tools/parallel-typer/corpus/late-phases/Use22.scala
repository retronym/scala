package lp.use22
import lp._

object Use22 {
  def m(s: Shape): String = s match { case Circle(r) if r > 22 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 22 }
  def miss(s: Shape): Int = s match { case _: Circle => 22 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 22 }
  def spec = { val b = new Box(22); val c = b.map(_ * 2); new IntBox(22).get + c.get + new Sum().run(Array(22)) + SpecUse.poly(22).get + Spec.id(22) }
  class Mine extends Box[Long](22.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 22 }
  class M22 extends C2 with Stack1 { override def log(s: String) = super.log(s + 22) ; override lazy val slow = "s22" }
  trait T22 extends WithVal { val mine = tag + 22; def ms = secret + slow.length }
  object O22 extends M22 with T22
  def lam = { var v = 22; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(22) + Lam.localDefs(v) }
  def inner = { val o = new Sub(22); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(22) + Meter.Rich(22).m; val w = new Wrap(22).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 22; def foo(i: Int) = "s" })
}
class Sub22 extends Outer(22) { class X extends In1 { override def f = super.f + 22 }; lazy val x = new X }
case class CC22(a: Int, b: String = "d") extends Shape022 { def area = a * 22 }
sealed trait Shape022
object Dup22 { def apply(i: Int) = CC22(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 22 else go(n - 1) }
