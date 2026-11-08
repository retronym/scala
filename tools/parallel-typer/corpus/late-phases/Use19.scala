package lp.use19
import lp._

object Use19 {
  def m(s: Shape): String = s match { case Circle(r) if r > 19 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 19 }
  def miss(s: Shape): Int = s match { case _: Circle => 19 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 19 }
  def spec = { val b = new Box(19); val c = b.map(_ * 2); new IntBox(19).get + c.get + new Sum().run(Array(19)) + SpecUse.poly(19).get + Spec.id(19) }
  class Mine extends Box[Long](19.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 19 }
  class M19 extends C2 with Stack1 { override def log(s: String) = super.log(s + 19) ; override lazy val slow = "s19" }
  trait T19 extends WithVal { val mine = tag + 19; def ms = secret + slow.length }
  object O19 extends M19 with T19
  def lam = { var v = 19; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(19) + Lam.localDefs(v) }
  def inner = { val o = new Sub(19); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(19) + Meter.Rich(19).m; val w = new Wrap(19).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 19; def foo(i: Int) = "s" })
}
class Sub19 extends Outer(19) { class X extends In1 { override def f = super.f + 19 }; lazy val x = new X }
case class CC19(a: Int, b: String = "d") extends Shape019 { def area = a * 19 }
sealed trait Shape019
object Dup19 { def apply(i: Int) = CC19(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 19 else go(n - 1) }
