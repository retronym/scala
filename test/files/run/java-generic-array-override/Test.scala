class S2 extends J[String] { override def g(xs: Array[String]): String = "S2.g" }
object Test {
  def main(a: Array[String]): Unit = {
    val s = new S2
    println(s.g(Array("a")))
    println((s: J[String]).g(Array("a")))
    println(J.call(s, Array("a")))
  }
}
