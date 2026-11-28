object Test {
  def main(args: Array[String]): Unit = {
    val r = new R(1, 2, 3)
    val l = List(r.rest)
    println(l.head.length)
  }
}
