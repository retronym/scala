// A partial function literal in a body that specialization duplicates (scala/bug#8633).

class C {
  def m[@specialized(Int) B](p: B): Option[B] =
    ({ case x if x > 1 => p }: PartialFunction[Int, B]).lift(2)
  def n[@specialized(Int) B](p: B, ys: List[Int]): List[B] =
    ys.collect { case 1 => p }
}
class D[@specialized(Int) B](p: B) {
  def m(ys: List[Any], q: B): B = ys.collectFirst { case s: String => p }.getOrElse(q)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().m(1))
    println(new C().m("s"))
    println(new C().n(1, List(1, 2, 1)))
    println(new D(1).m(List(2, "a"), 3))
    println(new D(1).m(List(2), 3))
  }
}
