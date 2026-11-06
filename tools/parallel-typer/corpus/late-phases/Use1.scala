package lp.use1
import lp._

object Use1 {
  def m(s: Shape): String = s match { case Circle(r) if r > 1 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 1 }
  def miss(s: Shape): Int = s match { case _: Circle => 1 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 1 }
  def spec = { val b = new Box(1); val c = b.map(_ * 2); new IntBox(1).get + c.get + new Sum().run(Array(1)) + SpecUse.poly(1).get + Spec.id(1) }
  class Mine extends Box[Long](1.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 1 }
  class M1 extends C2 with Stack1 { override def log(s: String) = super.log(s + 1) ; override lazy val slow = "s1" }
  trait T1 extends WithVal { val mine = tag + 1; def ms = secret + slow.length }
  object O1 extends M1 with T1
  def lam = { var v = 1; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(1) + Lam.localDefs(v) }
  def inner = { val o = new Sub(1); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(1) + Meter.Rich(1).m; val w = new Wrap(1).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 1; def foo(i: Int) = "s" })
}
class Sub1 extends Outer(1) { class X extends In1 { override def f = super.f + 1 }; lazy val x = new X }
case class CC1(a: Int, b: String = "d") extends Shape01 { def area = a * 1 }
sealed trait Shape01
object Dup1 { def apply(i: Int) = CC1(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 1 else go(n - 1) }
