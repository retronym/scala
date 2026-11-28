class C {
  def m[@specialized(Int) B](p: B): Option[B] =
    ({ case x if x > 1 => p }: PartialFunction[Int, B]).lift(2)
}
object Test { def main(a: Array[String]): Unit = println(new C().m(1)) }
