package lp.use3
import lp._

object Use3 {
  def m(s: Shape): String = s match { case Circle(r) if r > 3 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 3 }
  def miss(s: Shape): Int = s match { case _: Circle => 3 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 3 }
  def spec = { val b = new Box(3); val c = b.map(_ * 2); new IntBox(3).get + c.get + new Sum().run(Array(3)) + SpecUse.poly(3).get + Spec.id(3) }
  class Mine extends Box[Long](3.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 3 }
  class M3 extends C2 with Stack1 { override def log(s: String) = super.log(s + 3) ; override lazy val slow = "s3" }
  trait T3 extends WithVal { val mine = tag + 3; def ms = secret + slow.length }
  object O3 extends M3 with T3
  def lam = { var v = 3; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(3) + Lam.localDefs(v) }
  def inner = { val o = new Sub(3); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(3) + Meter.Rich(3).m; val w = new Wrap(3).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 3; def foo(i: Int) = "s" })
}
class Sub3 extends Outer(3) { class X extends In1 { override def f = super.f + 3 }; lazy val x = new X }
case class CC3(a: Int, b: String = "d") extends Shape03 { def area = a * 3 }
sealed trait Shape03
object Dup3 { def apply(i: Int) = CC3(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 3 else go(n - 1) }
