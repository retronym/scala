package cc

object Use23 {
  def a = P8(1).bump.copy(y = "u23")                      // inferred: P8
  def b = Q3("q", List(P3(2), P3(3, "z")))             // inferred: Q3[String]
  def c: Boolean = a == P8(2, "u23") && a.hashCode == P8(2, "u23").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P8(x, y), Q3(s, ps @ (P3(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P8.unapply(a).map(_._1).getOrElse(0) + P8.tupled((1, "t")).x
  def g = E8.size(E8.R("r", E8.L(1))) + E3.size(E3.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use24.a
}
