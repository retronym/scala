package t11109

// Compiled together with the Java source of `JaVarArgs`.
object Mixed {
  def calls: List[Int] = {
    val jva = new JaVarArgs
    List(
      jva.serialize("a", "b"),
      jva.universalize(Universal, Universal),
      JaVarArgs.cmp("a", "b"),
      JaVarArgs.cmpWild("a", "b"),
      JaVarArgs.cln(Array(1), Array(2)),
      JaVarArgs.twoLevel[String, String]("a", "b"),
      JaVarArgs.twoLevelUnbounded[String, String]("a", "b"),
      JaVarArgs.unbounded(1, 2),
      new JaVarArgs.Ctor("a", "b").n,
    )
  }
}
