// A specialized instance stores a `private[this]` field of a specialized type in the specialized subclass.
// Code of the generic class (methods whose signature does not mention `T`, constructor statements, inner
// classes) must see that field too, and writes from either side must be visible to the other.

class Box[@specialized(Int) T](init: T) {
  val v: T = init
  private[this] val w: T = init
  val list: List[Any] = List(v, w)
  override def toString = "Box(" + w + ")"
}

class Acc[@specialized(Int, Double) T](zero: T)(implicit n: Numeric[T]) {
  val start: T = zero
  private[this] var sum: T = zero
  def add(t: T): Unit = sum = n.plus(sum, t)
  def result: T = sum
  def describe: String = "sum=" + sum
  def reset(): Unit = sum = start
  class Inner { def peek: Any = sum }
}

// no accessor-backed field, so the generic constructor is not guarded
class OnlyHidden[@specialized(Int) T](init: T) {
  private[this] val w: T = init
  private[this] var s: T = init
  def setSpec(t: T): T = { s = t; w }
  def setGeneric(t: Any): Unit = s = t.asInstanceOf[T]
  def get: Any = (w, s)
}

class VarParam[@specialized(Int) T](private[this] var c: T) {
  val a: T = c
  def set(t: T): Unit = c = t
  def get: Any = c
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = new Box(1)
    println((b, b.list))

    def acc[T: Numeric](zero: T, x: T, y: T) = {
      val a = new Acc(zero)
      a.add(x); a.add(y)
      val r1 = (a.result, a.describe, new a.Inner().peek)
      a.reset()
      a.add(y)
      println((r1, a.result, a.describe))
    }
    acc(0, 5, 6)
    acc(0.0, 1.5, 2.0)
    acc(BigInt(0), BigInt(5), BigInt(6))

    val h = new OnlyHidden(1)
    println((h.setSpec(2), h.get))
    h.setGeneric(3)
    println((h.setSpec(4), h.get))

    val p = new VarParam(1)
    p.set(2)
    println((p.a, p.get))
  }
}
