package cc

object Use13 {
  def a = P6(1).bump.copy(y = "u13")                      // inferred: P6
  def b = Q1("q", List(P1(2), P1(3, "z")))             // inferred: Q1[String]
  def c: Boolean = a == P6(2, "u13") && a.hashCode == P6(2, "u13").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P6(x, y), Q1(s, ps @ (P1(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P6.unapply(a).map(_._1).getOrElse(0) + P6.tupled((1, "t")).x
  def g = E6.size(E6.R("r", E6.L(1))) + E1.size(E1.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use14.a
}
