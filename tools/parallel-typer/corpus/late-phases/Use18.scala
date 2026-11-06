package lp.use18
import lp._

object Use18 {
  def m(s: Shape): String = s match { case Circle(r) if r > 18 => "big"; case Circle(_) => "c"; case Rect(w, h) => "r" + w + h; case Empty => "e" }
  def t[A](x: Tree[A]): Int = x match { case Leaf(_) => 1; case Node(l, r) => t(l) + t(r); case Nope => 18 }
  def miss(s: Shape): Int = s match { case _: Circle => 18 }
  def e(s: Shape) = s match { case Extract(a, b) => a; case _ => 0.0 }
  val inner0 = new Inner0; def in(i: inner0.In) = i match { case inner0.In(n) => n + 18 }
  def spec = { val b = new Box(18); val c = b.map(_ * 2); new IntBox(18).get + c.get + new Sum().run(Array(18)) + SpecUse.poly(18).get + Spec.id(18) }
  class Mine extends Box[Long](18.toLong) with SpecMix[Long] { def t = get }
  class MineI extends Sum with Forward { def f(x: Int) = x + 18 }
  class M18 extends C2 with Stack1 { override def log(s: String) = super.log(s + 18) ; override lazy val slow = "s18" }
  trait T18 extends WithVal { val mine = tag + 18; def ms = secret + slow.length }
  object O18 extends M18 with T18
  def lam = { var v = 18; def add(x: Int) = v += x; List(1, 2).foreach(add); val f = Lam.compose((x: Int) => x + v, (y: Int) => y.toString); f(18) + Lam.localDefs(v) }
  def inner = { val o = new Sub(18); val i = new o.In1; val j = new i.In2; j.g + j.Deep.h + o.InO.c + new Top.Nested.NN().t + Top.Nested.Most.v.t }
  def value = { val a = new Meter(18) + Meter.Rich(18).m; val w = new Wrap(18).map(_ + 1); a.scale(2).toString + w.viaHidden + Tag("t").s + new VImpl().id(a).v }
  def jv = { val s = new ScalaJ; s.get + s.r + s.c(JBase.Colour.RED) + s.deep + new ScalaII().extra + s.items().size }
  def structural = Lam.structural(new { def size = 18; def foo(i: Int) = "s" })
}
class Sub18 extends Outer(18) { class X extends In1 { override def f = super.f + 18 }; lazy val x = new X }
case class CC18(a: Int, b: String = "d") extends Shape018 { def area = a * 18 }
sealed trait Shape018
object Dup18 { def apply(i: Int) = CC18(i); @annotation.tailrec def go(n: Int): Int = if (n <= 0) 18 else go(n - 1) }
