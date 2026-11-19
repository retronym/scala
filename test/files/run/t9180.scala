import scala.annotation.tailrec

// A tail call on another instance becomes a jump that stores that instance in the `this` slot.
// In the copy of the method in the specialized subclass, the receiver need not be an instance of that
// subclass: it may be a plain `Spec[Int]` created in generic code.
class Spec[@specialized(Int) T](val t: T) {
  @tailrec final def g(o: Spec[T], n: Int): T = if (n == 0) t else o.g(this, n - 1)
  @tailrec final def h(o: Spec[T], n: Int)(k: Int): String = if (n == 0) s"$t $k" else o.h(this, n - 1)(k + 1)
}

class ConsList[@specialized(Int) A](head: A, tail: ConsList[A]) {
  @tailrec final def mkString(sb: StringBuilder): String = {
    sb.append(head)
    if (tail == null) sb.toString
    else { sb.append(", "); tail.mkString(sb) }
  }
}

object Test {
  def generic[T](t: T) = new Spec[T](t)
  def makeEnd[T](t: T) = new ConsList[T](t, null)

  def main(args: Array[String]): Unit = {
    val a = new Spec(1)
    val b = new Spec(2)
    println(a.g(b, 0))
    println(a.g(b, 1))
    println(a.g(b, 100000))
    println(a.g(generic(3), 1))
    println(generic(4).g(a, 1))
    println(a.g(generic(5), 100001))
    println(a.h(generic(6), 3)(0))
    println(new Spec("x").g(new Spec("y"), 3))
    println(new ConsList(1, new ConsList(2, new ConsList(3, makeEnd(4)))).mkString(new StringBuilder))
    try a.g(null, 1) catch { case _: NullPointerException => println("NPE") }
  }
}
