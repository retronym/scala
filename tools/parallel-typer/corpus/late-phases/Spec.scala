package lp

class Box[@specialized(Int, Long, Double) T](val v: T) {
  def map[@specialized(Int, Double) U](f: T => U): Box[U] = new Box[U](f(v))
  def get: T = v
  private[this] val cached: T = v
  def same(o: T): Boolean = cached == o
  class In(val w: T) { def outerV: T = v }
  def in(w: T): In = new In(w)
}
trait Fold[@specialized(Int, Long) A, @specialized(Int) B] {
  def zero: B
  def step(b: B, a: A): B
  def run(xs: Array[A]): B = { var acc = zero; var i = 0; while (i < xs.length) { acc = step(acc, xs(i)); i += 1 }; acc }
}
abstract class Pair[@specialized(Int) A, B](val a: A, val b: B) { def left: A = a }
object Spec {
  def id[@specialized T](t: T): T = t
  def both[@specialized(Int, Boolean) A, @specialized(Int) B](a: A, b: B): (A, B) = (a, b)
  def arr[@specialized(Int, AnyRef) T <: Any](x: Array[T], i: Int): T = x(i)
}
