import scala.annotation.tailrec

// As t9180, for a private method. Specialization makes the method protected, so that the
// specialized subclass can override it. The tail jump on another instance is guarded in the copy
// of the method in that subclass, which needs to call the method on a plain `Spec[Int]` as well.
class Spec[@specialized(Int) T](val t: T) {
  @tailrec private def g(o: Spec[T], n: Int): T = if (n == 0) t else o.g(this, n - 1)
  def runG(o: Spec[T], n: Int): T = g(o, n)
}

object Test {
  def generic[T](t: T) = new Spec[T](t)

  def main(args: Array[String]): Unit = {
    val a = new Spec(1)
    val b = new Spec(2)
    println(a.runG(b, 0))
    println(a.runG(b, 1))
    println(a.runG(b, 100000))
    println(a.runG(generic(3), 1))
    println(generic(4).runG(a, 1))
    println(a.runG(generic(5), 100001))
    println(new Spec("x").runG(new Spec("y"), 3))
  }
}
