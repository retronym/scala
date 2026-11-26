object Test {
  final val Debug = false
  def g(xs: Int*): Int = xs.sum
  def f(xs: Seq[Int]): Int = if (Debug) -1 else g(xs: _*)
  def h(xs: Seq[Int]): String = if (true) String.format("%s-%s", xs.map(Int.box): _*) else ""
  def main(args: Array[String]): Unit = println(f(Seq(1, 2, 3)) + " " + h(Seq(4, 5)))
}
