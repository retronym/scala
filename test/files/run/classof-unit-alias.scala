object Ob { type U = Unit; type I = Int }

object Test {
  def main(args: Array[String]): Unit = {
    println(s"${classOf[Unit]} ${classOf[Ob.U]}")
    println(s"${classOf[Int]} ${classOf[Ob.I]}")
    val c: Class[Unit] = classOf[Ob.U]
    println(c == classOf[Unit])
  }
}
