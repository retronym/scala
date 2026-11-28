case class V0(u: Int) extends AnyVal
object Test {
  def m(a: Int, b: Int): Boolean = (if (a > 0) V0(b) else V0(7)) == V0(5)
  def p(a: Int, b: Int): Int = (if (a > 0) V0(b) else V0(7)).u
  def main(args: Array[String]): Unit = println(List(m(1, 5), m(0, 5), p(1, 4), p(0, 4)))
}
