package lp.use17
import lp._

object Use17 {
  def m(s: Shape): String = s match { case Circle(r) if r > 17 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 17 }
  def miss(s: Shape): Int = s match { case _: Circle => 17 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 17 }
  def spec = { val b = new Box(17); val c = b.map(_ * 2); new IntBox(17).get + c.get + new Sum().run(Array(17)) + SpecUse.poly(17).get + Spec.id(17) }
  class Mine extends Box[Long](17.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 17 }
  class M17 extends C2 with Stack1 { override def log(s: String) = super.log(s + 17) ; override lazy val slow = "s17" }
  trait T17 extends WithVal { val mine = tag + 17; def ms = secret + slow.length }
  object O17 extends M17 with T17
  def lam = { var v = 17; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(17) + Lam.localDefs(v) }
  def inner = { val o = new Sub(17); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(17) + Meter.Rich(17).m; val w = new Wrap(17).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 17; def foo(i: Int) = "s" })
}
class Sub17 extends Outer(17) { class X extends In1 { override def f = super.f + 17 }; lazy val x = new X }
case class CC17(a: Int, b: String = "d") extends Shape017 { def area = a * 17 }
sealed trait Shape017
object Dup17 { def apply(i: Int) = CC17(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 17 else go(n - 1) }
