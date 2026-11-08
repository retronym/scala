package lp

class IntBox(i: Int) extends Box[Int](i) { override def get: Int = super.get + 1 }
class GenBox[T](t: T) extends Box[T](t)
class Sum extends Fold[Int, Int] { def zero = 0; def step(b: Int, a: Int) = b + a }
class Cnt[A] extends Fold[A, Int] { def zero = 0; def step(b: Int, a: A) = b + 1 }
class IP(i: Int) extends Pair[Int, String](i, "s")
trait SpecMix[@specialized(Int) T] { def t: T; def twice: (T, T) = (t, t) }
class SM1 extends Box[Int](1) with SpecMix[Int] { def t = get }
object SpecUse {
  val b = new Box(1).map(_ + 1.0).map(d => d.toString)
  val s = new Sum().run(Array(1, 2, 3))
  val p = Spec.both(1, 2)
  def poly[@specialized(Int) X](x: X): Box[X] = new Box(x)
}
