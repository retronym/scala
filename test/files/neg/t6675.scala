//> using options -deprecation -Xfatal-warnings
//
object X {
  def unapply(s: String): Option[(Int,Int,Int)] = Some((1,2,3))
}

object Y {
  def unapplySeq(s: String): Option[Seq[(Int,Int,Int)]] = Some(Seq((1,2,3)))
}

object Test {
  "" match { case X(b) => b case x => throw new MatchError(x) } // should warn under -Xlint. Not an error because of scala/bug#6111

  "" match { case Y(b) => b case x => throw new MatchError(x) } // no warning
}
