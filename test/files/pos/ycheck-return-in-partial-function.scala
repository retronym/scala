//> using options -Ycheck:all -Werror

object Test {
  def m(p: Int): Option[Double] = {
    val y: Int = ({ case x if x > 1 => if (x > 7) return Option(0.5) else 1 }: PartialFunction[Int, Int]).applyOrElse(p, (x: Int) => 2)
    Some(y.toDouble)
  }
}
