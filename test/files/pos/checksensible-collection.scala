//> using options -Werror

trait Marker

class MySeq extends scala.collection.immutable.AbstractSeq[Int] with Marker {
  def apply(i: Int) = 1
  def length = 1
  def iterator = Iterator(1)
}

// The other operand is a trait that a collection can implement: each comparison can be true.
class Test {
  def product(xs: List[Int], p: Product) = xs == p
  def serializable(xs: Seq[Int], s: java.io.Serializable) = xs == s
  def marker(xs: List[Int], m: Marker) = xs == m
  def markerNe(xs: Vector[Int], m: Marker) = xs != m

  def check(): Unit = {
    assert(product(List(1), new ::(1, Nil)))
    assert(serializable(List(1), List(1)))
    assert(marker(List(1), new MySeq))
    assert(!markerNe(Vector(1), new MySeq))
  }
}
