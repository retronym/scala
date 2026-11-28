object Test {
  def m(p: Int): Option[Double] = { val y: Int = ({ case x if x > 1 => if (x > 7) return Option(0.5) else 1 }: PartialFunction[Int, Int]).applyOrElse(p, (x: Int) => 2); Some(y.toDouble) }
  def main(a: Array[String]): Unit = println(List(m(0), m(3), m(9)))
}
