package lp.use21
import lp._

object Use21 {
  def m(s: Shape): String = s match { case Circle(r) if r > 21 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 21 }
  def miss(s: Shape): Int = s match { case _: Circle => 21 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 21 }
  def spec = { val b = new Box(21); val c = b.map(_ * 2); new IntBox(21).get + c.get + new Sum().run(Array(21)) + SpecUse.poly(21).get + Spec.id(21) }
  class Mine extends Box[Long](21.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 21 }
  class M21 extends C2 with Stack1 { override def log(s: String) = super.log(s + 21) ; override lazy val slow = "s21" }
  trait T21 extends WithVal { val mine = tag + 21; def ms = secret + slow.length }
  object O21 extends M21 with T21
  def lam = { var v = 21; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(21) + Lam.localDefs(v) }
  def inner = { val o = new Sub(21); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(21) + Meter.Rich(21).m; val w = new Wrap(21).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 21; def foo(i: Int) = "s" })
}
class Sub21 extends Outer(21) { class X extends In1 { override def f = super.f + 21 }; lazy val x = new X }
case class CC21(a: Int, b: String = "d") extends Shape021 { def area = a * 21 }
sealed trait Shape021
object Dup21 { def apply(i: Int) = CC21(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 21 else go(n - 1) }
