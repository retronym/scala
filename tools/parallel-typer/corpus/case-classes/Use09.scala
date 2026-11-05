package cc

object Use09 {
  def a = P2(1).bump.copy(y = "u09")                      // inferred: P2
  def b = Q5("q", List(P5(2), P5(3, "z")))             // inferred: Q5[String]
  def c: Boolean = a == P2(2, "u09") && a.hashCode == P2(2, "u09").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P2(x, y), Q5(s, ps @ (P5(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P2.unapply(a).map(_._1).getOrElse(0) + P2.tupled((1, "t")).x
  def g = E2.size(E2.R("r", E2.L(1))) + E5.size(E5.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use10.a
}
