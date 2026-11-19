package lp.use15
import lp._

object Use15 {
  def m(s: Shape): String = s match { case Circle(r) if r > 15 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 15 }
  def miss(s: Shape): Int = s match { case _: Circle => 15 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 15 }
  def spec = { val b = new Box(15); val c = b.map(_ * 2); new IntBox(15).get + c.get + new Sum().run(Array(15)) + SpecUse.poly(15).get + Spec.id(15) }
  class Mine extends Box[Long](15.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 15 }
  class M15 extends C2 with Stack1 { override def log(s: String) = super.log(s + 15) ; override lazy val slow = "s15" }
  trait T15 extends WithVal { val mine = tag + 15; def ms = secret + slow.length }
  object O15 extends M15 with T15
  def lam = { var v = 15; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(15) + Lam.localDefs(v) }
  def inner = { val o = new Sub(15); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(15) + Meter.Rich(15).m; val w = new Wrap(15).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 15; def foo(i: Int) = "s" })
}
class Sub15 extends Outer(15) { class X extends In1 { override def f = super.f + 15 }; lazy val x = new X }
case class CC15(a: Int, b: String = "d") extends Shape015 { def area = a * 15 }
sealed trait Shape015
object Dup15 { def apply(i: Int) = CC15(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 15 else go(n - 1) }
