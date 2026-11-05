package cc

object Use20 {
  def a = P5(1).bump.copy(y = "u20")                      // inferred: P5
  def b = Q8("q", List(P8(2), P8(3, "z")))             // inferred: Q8[String]
  def c: Boolean = a == P5(2, "u20") && a.hashCode == P5(2, "u20").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P5(x, y), Q8(s, ps @ (P8(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P5.unapply(a).map(_._1).getOrElse(0) + P5.tupled((1, "t")).x
  def g = E5.size(E5.R("r", E5.L(1))) + E8.size(E8.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use21.a
}
