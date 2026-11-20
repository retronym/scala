import t11109._

class SI extends JaVarArgs.I { def m[U <: Comparable[U]](us: U*) = us.length }
class SAC[T <: Comparable[T]] extends JaVarArgs.AC[T] { def n(ts: T*) = ts.length }

// Compiled against the class files of `JaVarArgs`.
object Test {
  def gen[X <: Comparable[X]](x: X) = JaVarArgs.cmp(x, x)

  def main(args: Array[String]): Unit = {
    val jva = new JaVarArgs
    val calls = List(
      jva.serialize("a", "b"),
      jva.universalize(Universal, Universal),
      JaVarArgs.cmp("a", "b"),
      JaVarArgs.cmp(Seq("a", "b"): _*),
      JaVarArgs.cmp[Integer](1, 2),
      JaVarArgs.cmpSuper("a", "b"),
      JaVarArgs.cmpWild("a", "b"),
      JaVarArgs.cln(Array(1), Array(2)),
      JaVarArgs.twoLevel[String, String]("a", "b"),
      JaVarArgs.twoLevelUnbounded[String, String]("a", "b"),
      JaVarArgs.unbounded(1, 2),
      new JaVarArgs.Ctor("a", "b").n,
      gen("a"),
      (new SI: JaVarArgs.I).m("a", "b"),
      JaVarArgs.callI(new SI),
      (new SAC[String]: JaVarArgs.AC[String]).n("a", "b"),
      JaVarArgs.callAC(new SAC[String]),
    )
    assert(calls.forall(_ == 2), calls)
    assert(Mixed.calls.forall(_ == 2), Mixed.calls)
  }
}
