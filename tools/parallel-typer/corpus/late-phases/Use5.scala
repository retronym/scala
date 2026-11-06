package lp.use5
import lp._

object Use5 {
  def m(s: Shape): String = s match { case Circle(r) if r > 5 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 5 }
  def miss(s: Shape): Int = s match { case _: Circle => 5 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 5 }
  def spec = { val b = new Box(5); val c = b.map(_ * 2); new IntBox(5).get + c.get + new Sum().run(Array(5)) + SpecUse.poly(5).get + Spec.id(5) }
  class Mine extends Box[Long](5.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 5 }
  class M5 extends C2 with Stack1 { override def log(s: String) = super.log(s + 5) ; override lazy val slow = "s5" }
  trait T5 extends WithVal { val mine = tag + 5; def ms = secret + slow.length }
  object O5 extends M5 with T5
  def lam = { var v = 5; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(5) + Lam.localDefs(v) }
  def inner = { val o = new Sub(5); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(5) + Meter.Rich(5).m; val w = new Wrap(5).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 5; def foo(i: Int) = "s" })
}
class Sub5 extends Outer(5) { class X extends In1 { override def f = super.f + 5 }; lazy val x = new X }
case class CC5(a: Int, b: String = "d") extends Shape05 { def area = a * 5 }
sealed trait Shape05
object Dup5 { def apply(i: Int) = CC5(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 5 else go(n - 1) }
