case class C(private val p: Int, p$access: String)

object Test {
  def main(args: Array[String]): Unit = {
    val c = C(1, "s")
    println(c)
    println(c.productElement(0))
    println(c.productElementName(0))
    println(C.unapply(c))
    c match { case C(a, b) => println((a, b)) }
  }
}
