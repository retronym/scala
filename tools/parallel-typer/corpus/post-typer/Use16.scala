package pt.use16

import pt._
import pt.Syntax._

// super calls from a trait, an inner class and a closure: super accessors in this file's classes
class Sub16 extends Base with Stack1 with Stack2 {
  override def m(x: Int): Int = super.m(x) + 16
  override def over(x: Int): Int = super.over(x) - 16
  def viaClosure(xs: List[Int]): List[Int] = xs.map(x => super.m(x))
  def viaMix(x: Int): Int = super[Stack1].m(x) + super[Stack2].m(x)
  class Inner { def call(x: Int): Int = Sub16.super.m(x) + Sub16.super[Stack1].gen(x) }
  override protected def prot(s: String): String = super.prot(s) + "16"
  def viaProt(xs: List[String]): List[String] = xs.map(s => super.prot(s))
}

trait Mix16 extends Base {
  override def m(x: Int): Int = super.m(x) - 16
  def twice(x: Int): Int = { val f = (y: Int) => super.m(y); f(f(x)) }
}

// protected Java members from closures and inner classes of a class in another package: protected accessors
class J16 extends ptj.JBase with ptj.JGreeter {
  def name: String = "J16"
  def greeters(ws: List[String]): List[String] = ws.map(w => greet(w))
  def all(ps: List[String]): List[String] = ps.map(p => describe(p))
  def picked[T](xs: List[T], d: T): List[T] = xs.map(x => pick(x, d))
  def set(vs: List[Int]): Unit = vs.foreach(v => counter = v + 16)
  class Reader { def read: Int = counter + bump(); def label: String = describe("r") }
}

class Q16 extends Qualified {
  val z = y
  def both: String = z + shown(16)
  val inner = new Inner
  def touched: Int = inner.touch()
}

object Values16 {
  val length: Meters = 16.meters + Meters.of(1.5).scale(2).scale(0.5) + Meters.Zero
  val id: Id = "use16".id.next
  val shown: String = id.show + new Wrapped(16).map(_ + 1).zip("w".wrapped).shown
  def count: Int = id.countdown(16)
  def folded: Int = new Wrapped(List(16)).fold(0)(_ + _.sum)
  class Local(val v: Int) { def lifted: List[Meters] = v.times(v.meters) }
  def inl(i: Inlined): Int = i.inc() + i.inc()
  def shape(s: Shape): Double = s match { case Circle(r) => r; case Square(x) => x; case Empty => 0 }
  val p = new Pickled[Int](16)
  def refined = p.refined.name + p.first + p.Const
  def java: Int = ptj.JUsesScala.answer() + new ptj.JUsesScala().m(16)
}

// its own value class, companion and extension methods
class Tag16(val n: Int) extends AnyVal {
  def +(o: Tag16): Tag16 = new Tag16(n + o.n)
  def meters: Meters = n.meters
}
object Tag16 { implicit val ord: Ordering[Tag16] = Ordering.by(_.n) }
