object Test {
  def foo: Seq[Int] = Vector(1)

  def main(args: Array[String]) {
    def x = foo match {
      case x :: Nil => x
      case _ => 
    }
    println(x)
    ()
  }
}
