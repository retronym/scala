package lp.use7
import lp._

object Use7 {
  def m(s: Shape): String = s match { case Circle(r) if r > 7 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 7 }
  def miss(s: Shape): Int = s match { case _: Circle => 7 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 7 }
  def spec = { val b = new Box(7); val c = b.map(_ * 2); new IntBox(7).get + c.get + new Sum().run(Array(7)) + SpecUse.poly(7).get + Spec.id(7) }
  class Mine extends Box[Long](7.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 7 }
  class M7 extends C2 with Stack1 { override def log(s: String) = super.log(s + 7) ; override lazy val slow = "s7" }
  trait T7 extends WithVal { val mine = tag + 7; def ms = secret + slow.length }
  object O7 extends M7 with T7
  def lam = { var v = 7; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(7) + Lam.localDefs(v) }
  def inner = { val o = new Sub(7); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(7) + Meter.Rich(7).m; val w = new Wrap(7).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 7; def foo(i: Int) = "s" })
}
class Sub7 extends Outer(7) { class X extends In1 { override def f = super.f + 7 }; lazy val x = new X }
case class CC7(a: Int, b: String = "d") extends Shape07 { def area = a * 7 }
sealed trait Shape07
object Dup7 { def apply(i: Int) = CC7(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 7 else go(n - 1) }
