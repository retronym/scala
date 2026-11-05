package cc

object Use03 {
  def a = P4(1).bump.copy(y = "u03")                      // inferred: P4
  def b = Q7("q", List(P7(2), P7(3, "z")))             // inferred: Q7[String]
  def c: Boolean = a == P4(2, "u03") && a.hashCode == P4(2, "u03").hashCode && a.equals(a.bump)
  def d: List[Any] = a.productIterator.toList ++ b.productIterator.toList
  def e = (a, b) match {
    case (P4(x, y), Q7(s, ps @ (P7(n, _) :: _))) => x + y.length + s.length + n + ps.size
    case _ => -1
  }
  def f = P4.unapply(a).map(_._1).getOrElse(0) + P4.tupled((1, "t")).x
  def g = E4.size(E4.R("r", E4.L(1))) + E7.size(E7.Z)
  def h = Seq(a, a.bump).map(_.toString).mkString + a.productElementName(0) + b.canEqual(b)
  def i = Use04.a
}
