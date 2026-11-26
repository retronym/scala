class C {
  def m[@specialized(Int) B](p: B): Option[B] = {
    lazy val x: B = p
    def loc(q: Int): B => B = (y: B) => x
    Option[B](x)
  }
}
object Test { def main(a: Array[String]): Unit = println(new C().m(1)) }
