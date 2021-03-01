object Test {
  class C {
    def testValDef = Macro.valDef({val c = "c"; c})
    def testDefDef = Macro.defDef({val c = "c"; c})
    def testLambda = Macro.lambda({val c = "c"; c})
  }

  def main(args: Array[String]): Unit = {
    val c = new C
    assert(c.testValDef == "c")
    assert(c.testDefDef == "c")
    assert(c.testLambda == "c")
  }
}
