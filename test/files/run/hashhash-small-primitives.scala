object Test {
  def main(args: Array[String]): Unit = {
    val c = 'a'
    val b: Byte = 3
    val s: Short = 4
    println(c.##.toString)               // 97
    println("" + c.##)                   // 97
    val o: Any = c.##
    println(o.getClass.getName)          // java.lang.Integer
    println(o.isInstanceOf[Int])         // true
    println((b.## : Any).getClass.getName)
    println((s.## : Any).getClass.getName)
  }
}
