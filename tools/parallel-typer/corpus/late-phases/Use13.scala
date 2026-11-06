package lp.use13
import lp._

object Use13 {
  def m(s: Shape): String = s match { case Circle(r) if r > 13 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 13 }
  def miss(s: Shape): Int = s match { case _: Circle => 13 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 13 }
  def spec = { val b = new Box(13); val c = b.map(_ * 2); new IntBox(13).get + c.get + new Sum().run(Array(13)) + SpecUse.poly(13).get + Spec.id(13) }
  class Mine extends Box[Long](13.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 13 }
  class M13 extends C2 with Stack1 { override def log(s: String) = super.log(s + 13) ; override lazy val slow = "s13" }
  trait T13 extends WithVal { val mine = tag + 13; def ms = secret + slow.length }
  object O13 extends M13 with T13
  def lam = { var v = 13; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(13) + Lam.localDefs(v) }
  def inner = { val o = new Sub(13); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(13) + Meter.Rich(13).m; val w = new Wrap(13).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 13; def foo(i: Int) = "s" })
}
class Sub13 extends Outer(13) { class X extends In1 { override def f = super.f + 13 }; lazy val x = new X }
case class CC13(a: Int, b: String = "d") extends Shape013 { def area = a * 13 }
sealed trait Shape013
object Dup13 { def apply(i: Int) = CC13(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 13 else go(n - 1) }
