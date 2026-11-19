package lp.use10
import lp._

object Use10 {
  def m(s: Shape): String = s match { case Circle(r) if r > 10 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 10 }
  def miss(s: Shape): Int = s match { case _: Circle => 10 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 10 }
  def spec = { val b = new Box(10); val c = b.map(_ * 2); new IntBox(10).get + c.get + new Sum().run(Array(10)) + SpecUse.poly(10).get + Spec.id(10) }
  class Mine extends Box[Long](10.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 10 }
  class M10 extends C2 with Stack1 { override def log(s: String) = super.log(s + 10) ; override lazy val slow = "s10" }
  trait T10 extends WithVal { val mine = tag + 10; def ms = secret + slow.length }
  object O10 extends M10 with T10
  def lam = { var v = 10; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(10) + Lam.localDefs(v) }
  def inner = { val o = new Sub(10); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(10) + Meter.Rich(10).m; val w = new Wrap(10).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 10; def foo(i: Int) = "s" })
}
class Sub10 extends Outer(10) { class X extends In1 { override def f = super.f + 10 }; lazy val x = new X }
case class CC10(a: Int, b: String = "d") extends Shape010 { def area = a * 10 }
sealed trait Shape010
object Dup10 { def apply(i: Int) = CC10(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 10 else go(n - 1) }
