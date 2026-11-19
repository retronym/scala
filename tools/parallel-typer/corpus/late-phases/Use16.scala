package lp.use16
import lp._

object Use16 {
  def m(s: Shape): String = s match { case Circle(r) if r > 16 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 16 }
  def miss(s: Shape): Int = s match { case _: Circle => 16 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 16 }
  def spec = { val b = new Box(16); val c = b.map(_ * 2); new IntBox(16).get + c.get + new Sum().run(Array(16)) + SpecUse.poly(16).get + Spec.id(16) }
  class Mine extends Box[Long](16.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 16 }
  class M16 extends C2 with Stack1 { override def log(s: String) = super.log(s + 16) ; override lazy val slow = "s16" }
  trait T16 extends WithVal { val mine = tag + 16; def ms = secret + slow.length }
  object O16 extends M16 with T16
  def lam = { var v = 16; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(16) + Lam.localDefs(v) }
  def inner = { val o = new Sub(16); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(16) + Meter.Rich(16).m; val w = new Wrap(16).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 16; def foo(i: Int) = "s" })
}
class Sub16 extends Outer(16) { class X extends In1 { override def f = super.f + 16 }; lazy val x = new X }
case class CC16(a: Int, b: String = "d") extends Shape016 { def area = a * 16 }
sealed trait Shape016
object Dup16 { def apply(i: Int) = CC16(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 16 else go(n - 1) }
