package cc

object Use10 {
  def a = P3(1).bump.copy(y = "u10")                      // inferred: P3
  def b = Q6("q", List(P6(2), P6(3, "z")))             // inferred: Q6[String]
  def c: Boolean = a == P3(2, "u10") && a.hashCode == P3(2, "u10").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P3(x, y), Q6(s, ps @ (P6(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P3.unapply(a).map(_._1).getOrElse(0) + P3.tupled((1, "t")).x
  def g = E3.size(E3.R("r", E3.L(1))) + E6.size(E6.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use11.a
}
