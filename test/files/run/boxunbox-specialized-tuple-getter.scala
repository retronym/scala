//> using options -opt:local
object Test {
  // valid program: the cast to (Long, Long) is only reached when wide is true
  def sum(wide: Boolean): Long = {
    val t = (1, 2)
    val a: Any = t
    if (wide) { val u = a.asInstanceOf[(Long, Long)]; u._1 + u._2 }
    else { val u = a.asInstanceOf[(Int, Int)]; (u._1 + u._2).toLong }
  }
  def main(args: Array[String]): Unit = println(sum(false))
}
