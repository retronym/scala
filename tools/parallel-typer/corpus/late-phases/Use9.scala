package lp.use9
import lp._

object Use9 {
  def m(s: Shape): String = s match { case Circle(r) if r > 9 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 9 }
  def miss(s: Shape): Int = s match { case _: Circle => 9 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 9 }
  def spec = { val b = new Box(9); val c = b.map(_ * 2); new IntBox(9).get + c.get + new Sum().run(Array(9)) + SpecUse.poly(9).get + Spec.id(9) }
  class Mine extends Box[Long](9.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 9 }
  class M9 extends C2 with Stack1 { override def log(s: String) = super.log(s + 9) ; override lazy val slow = "s9" }
  trait T9 extends WithVal { val mine = tag + 9; def ms = secret + slow.length }
  object O9 extends M9 with T9
  def lam = { var v = 9; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(9) + Lam.localDefs(v) }
  def inner = { val o = new Sub(9); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(9) + Meter.Rich(9).m; val w = new Wrap(9).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 9; def foo(i: Int) = "s" })
}
class Sub9 extends Outer(9) { class X extends In1 { override def f = super.f + 9 }; lazy val x = new X }
case class CC9(a: Int, b: String = "d") extends Shape09 { def area = a * 9 }
sealed trait Shape09
object Dup9 { def apply(i: Int) = CC9(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 9 else go(n - 1) }
