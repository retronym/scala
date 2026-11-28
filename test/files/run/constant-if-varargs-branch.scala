object Test {
  final val On = true
  final val Off = false

  def g(xs: Int*): Int = xs.sum
  def thenBranchOn(xs: Seq[Int]): Int = if (On) g(xs: _*) else -1
  def elseBranchOff(xs: Seq[Int]): Int = if (Off) -1 else g(xs: _*)
  def thenBranchLit(xs: Seq[Int]): String = if (true) String.format("%s-%s", xs.map(Int.box): _*) else ""
  def elseBranchLit(xs: Seq[Int]): String = if (false) "" else String.format("%s-%s", xs.map(Int.box): _*)
  def asArgument(xs: Seq[Int]): Int = math.max(if (Off) -1 else g(xs: _*), if (On) g(xs: _*) else -1)

  def main(args: Array[String]): Unit = {
    println(thenBranchOn(Seq(1, 2, 3)))
    println(elseBranchOff(Seq(1, 2, 3)))
    println(thenBranchLit(Seq(4, 5)))
    println(elseBranchLit(Seq(4, 5)))
    println(asArgument(Seq(7, 8)))
  }
}
