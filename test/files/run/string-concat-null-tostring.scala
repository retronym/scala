class NullTS { override def toString: String = null }
object Test {
  def main(args: Array[String]): Unit = {
    val x = new NullTS
    val a: String = "" + x
    val b: String = "<" + x
    println(a == null)
    println(b)
    println(("" + x + "") == null)
  }
}
