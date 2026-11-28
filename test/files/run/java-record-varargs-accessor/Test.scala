//> using jvm 17+
object Test {
  def main(args: Array[String]): Unit = {
    val c = new Command("cp", "a", "b")
    val a: Array[String] = c.args
    println(a.length)
    println(c.args.toList)
    println(c.args.map(_.toUpperCase).mkString(","))
    println(List(c.args).head.length)
  }
}
