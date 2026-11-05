package pt.use12

import pt._
import pt.Syntax._

// super calls from a trait, an inner class and a closure: super accessors in this file's classes
class Sub12 extends Base with Stack1 with Stack2 {
  override def m(x: Int): Int = super.m(x) + 12
  override def over(x: Int): Int = super.over(x) - 12
  def viaClosure(xs: List[Int]): List[Int] = xs.map(x => super.m(x))
  def viaMix(x: Int): Int = super[Stack1].m(x) + super[Stack2].m(x)
  class Inner { def call(x: Int): Int = Sub12.super.m(x) + Sub12.super[Stack1].gen(x) }
  override protected def prot(s: String): String = super.prot(s) + "12"
  def viaProt(xs: List[String]): List[String] = xs.map(s => super.prot(s))
}

trait Mix12 extends Base {
  override def m(x: Int): Int = super.m(x) - 12
  def twice(x: Int): Int = { val f = (y: Int) => super.m(y); f(f(x)) }
}

// protected Java members from closures and inner classes of a class in another package: protected accessors
class J12 extends ptj.JBase with ptj.JGreeter {
  def name: String = "J12"
  def greeters(ws: List[String]): List[String] = ws.map(w => greet(w))
  def all(ps: List[String]): List[String] = ps.map(p => describe(p))
  def picked[T](xs: List[T], d: T): List[T] = xs.map(x => pick(x, d))
  def set(vs: List[Int]): Unit = vs.foreach(v => counter = v + 12)
  class Reader { def read: Int = counter + bump(); def label: String = describe("r") }
}

class Q12 extends Qualified {
  val z = y
  def both: String = z + shown(12)
  val inner = new Inner
  def touched: Int = inner.touch()
}

object Values12 {
  val length: Meters = 12.meters + Meters.of(1.5).scale(2).scale(0.5) + Meters.Zero
  val id: Id = "use12".id.next
  val shown: String = id.show + new Wrapped(12).map(_ + 1).zip("w".wrapped).shown
  def count: Int = id.countdown(12)
  def folded: Int = new Wrapped(List(12)).fold(0)(_ + _.sum)
  class Local(val v: Int) { def lifted: List[Meters] = v.times(v.meters) }
  def inl(i: Inlined): Int = i.inc() + i.inc()
  def shape(s: Shape): Double = s match { case Circle(r) => r; case Square(x) => x; case Empty => 0 }
  val p = new Pickled[Int](12)
  def refined = p.refined.name + p.first + p.Const
  def java: Int = ptj.JUsesScala.answer() + new ptj.JUsesScala().m(12)
}

// its own value class, companion and extension methods
class Tag12(val n: Int) extends AnyVal {
  def +(o: Tag12): Tag12 = new Tag12(n + o.n)
  def meters: Meters = n.meters
}
object Tag12 { implicit val ord: Ordering[Tag12] = Ordering.by(_.n) }
