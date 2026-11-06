package lp.use14
import lp._

object Use14 {
  def m(s: Shape): String = s match { case Circle(r) if r > 14 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 14 }
  def miss(s: Shape): Int = s match { case _: Circle => 14 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 14 }
  def spec = { val b = new Box(14); val c = b.map(_ * 2); new IntBox(14).get + c.get + new Sum().run(Array(14)) + SpecUse.poly(14).get + Spec.id(14) }
  class Mine extends Box[Long](14.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 14 }
  class M14 extends C2 with Stack1 { override def log(s: String) = super.log(s + 14) ; override lazy val slow = "s14" }
  trait T14 extends WithVal { val mine = tag + 14; def ms = secret + slow.length }
  object O14 extends M14 with T14
  def lam = { var v = 14; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(14) + Lam.localDefs(v) }
  def inner = { val o = new Sub(14); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(14) + Meter.Rich(14).m; val w = new Wrap(14).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 14; def foo(i: Int) = "s" })
}
class Sub14 extends Outer(14) { class X extends In1 { override def f = super.f + 14 }; lazy val x = new X }
case class CC14(a: Int, b: String = "d") extends Shape014 { def area = a * 14 }
sealed trait Shape014
object Dup14 { def apply(i: Int) = CC14(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 14 else go(n - 1) }
