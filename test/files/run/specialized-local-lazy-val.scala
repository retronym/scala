// A lambda that reads a local lazy val of the specialized type, in a body that specialization duplicates.

class C {
  def m[@specialized(Int) B](p: B): () => B = { lazy val x: B = p; () => x }
}
class D[@specialized(Int) B](p: B) {
  def m: () => B = { lazy val x: B = p; () => x }
}
class E {
  def m[@specialized(Int) B](p: B): Option[B] = {
    lazy val x: B = p
    def loc(q: Int): B => B = (y: B) => x
    Option[B](loc(0)(p))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().m(1)())
    println(new C().m("s")())
    println(new D(1).m())
    println(new D("s").m())
    println(new E().m(1))
  }
}
