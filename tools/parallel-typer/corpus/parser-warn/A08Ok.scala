package pwarn

object A08Ok {
  val t = (1, "two", 3.0)
  val f: (Int, String) => (Int, String) = (a, b) => (a, b)
  val (p, q) = (1, 2)
  def g(xs: List[Int]) = for { x <- xs; y = x + 1; if y > 2 } yield (x, y)
  val pf: PartialFunction[Any, Int] = { case (a: Int, _) => a }
  val any = List(1, 2).map(_ + 1)
}
