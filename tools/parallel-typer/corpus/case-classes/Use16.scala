package cc

object Use16 {
  def a = P1(1).bump.copy(y = "u16")                      // inferred: P1
  def b = Q4("q", List(P4(2), P4(3, "z")))             // inferred: Q4[String]
  def c: Boolean = a == P1(2, "u16") && a.hashCode == P1(2, "u16").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P1(x, y), Q4(s, ps @ (P4(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P1.unapply(a).map(_._1).getOrElse(0) + P1.tupled((1, "t")).x
  def g = E1.size(E1.R("r", E1.L(1))) + E4.size(E4.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use17.a
}
