package pt.use27

import pt._
import pt.Syntax._

// super calls from a trait, an inner class and a closure: super accessors in this file's classes
class Sub27 extends Base with Stack1 with Stack2 {
  override def m(x: Int): Int = super.m(x) + 27
  override def over(x: Int): Int = super.over(x) - 27
  def viaClosure(xs: List[Int]): List[Int] = xs.map(x => super.m(x))
  def viaMix(x: Int): Int = super[Stack1].m(x) + super[Stack2].m(x)
  class Inner { def call(x: Int): Int = Sub27.super.m(x) + Sub27.super[Stack1].gen(x) }
  override protected def prot(s: String): String = super.prot(s) + "27"
  def viaProt(xs: List[String]): List[String] = xs.map(s => super.prot(s))
}

trait Mix27 extends Base {
  override def m(x: Int): Int = super.m(x) - 27
  def twice(x: Int): Int = { val f = (y: Int) => super.m(y); f(f(x)) }
}

// protected Java members from closures and inner classes of a class in another package: protected accessors
class J27 extends ptj.JBase with ptj.JGreeter {
  def name: String = "J27"
  def greeters(ws: List[String]): List[String] = ws.map(w => greet(w))
  def all(ps: List[String]): List[String] = ps.map(p => describe(p))
  def picked[T](xs: List[T], d: T): List[T] = xs.map(x => pick(x, d))
  def set(vs: List[Int]): Unit = vs.foreach(v => counter = v + 27)
  class Reader { def read: Int = counter + bump(); def label: String = describe("r") }
}

class Q27 extends Qualified {
  val z = y
  def both: String = z + shown(27)
  val inner = new Inner
  def touched: Int = inner.touch()
}

object Values27 {
  val length: Meters = 27.meters + Meters.of(1.5).scale(2).scale(0.5) + Meters.Zero
  val id: Id = "use27".id.next
  val shown: String = id.show + new Wrapped(27).map(_ + 1).zip("w".wrapped).shown
  def count: Int = id.countdown(27)
  def folded: Int = new Wrapped(List(27)).fold(0)(_ + _.sum)
  class Local(val v: Int) { def lifted: List[Meters] = v.times(v.meters) }
  def inl(i: Inlined): Int = i.inc() + i.inc()
  def shape(s: Shape): Double = s match { case Circle(r) => r; case Square(x) => x; case Empty => 0 }
  val p = new Pickled[Int](27)
  def refined = p.refined.name + p.first + p.Const
  def java: Int = ptj.JUsesScala.answer() + new ptj.JUsesScala().m(27)
}

// its own value class, companion and extension methods
class Tag27(val n: Int) extends AnyVal {
  def +(o: Tag27): Tag27 = new Tag27(n + o.n)
  def meters: Meters = n.meters
}
object Tag27 { implicit val ord: Ordering[Tag27] = Ordering.by(_.n) }
