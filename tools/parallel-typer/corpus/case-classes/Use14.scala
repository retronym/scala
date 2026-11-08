package cc

object Use14 {
  def a = P7(1).bump.copy(y = "u14")                      // inferred: P7
  def b = Q2("q", List(P2(2), P2(3, "z")))             // inferred: Q2[String]
  def c: Boolean = a == P7(2, "u14") && a.hashCode == P7(2, "u14").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P7(x, y), Q2(s, ps @ (P2(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P7.unapply(a).map(_._1).getOrElse(0) + P7.tupled((1, "t")).x
  def g = E7.size(E7.R("r", E7.L(1))) + E2.size(E2.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use15.a
}
