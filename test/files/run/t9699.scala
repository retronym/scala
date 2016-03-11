import scala.tools.nsc.interactive, scala.tools.nsc.reporters._, scala.tools.nsc._

object Test {
  def main(args: Array[String]): Unit = {
    val reporter = new StoreReporter
    val s = new Settings()
    s.processArgumentString("-Ypresentation-any-thread")
    val global = new interactive.Global(s, reporter)
    import global._
    val Cursor = "" // "_DUMMY_ "
    def completions(before: String, after: String): List[String] = {
      val run = new global.TyperRun
      val unit = newCompilationUnit(before + Cursor + after)
      val richUnit = new RichCompilationUnit(unit.source)
      unitOfFile(richUnit.source.file) = richUnit
      val results = global.completionsAt(richUnit.position(before.length))
      results.matchingResults().map(_.symNameDropLocal.decoded).distinct
    }
    def check(actual: => Any, expected: Any) {
      try {
        if (actual != expected)
          println(s"NOK: $actual != $expected")
      } catch {
        case t: Throwable =>
          println("NOK: " + t.getMessage)
      }
    }
    val complex1 =
      """object Test {
        |class spanny(s: String) {
        |  def apply(d: String*) = {}
        |}
        |lazy val span = new spanny("spa")
        |def orange = span
        |println(orang""".stripMargin
    val complex2 =
      """e()
        |}
      """.stripMargin
    println(complex1 + Console.RED + "|" + Console.RESET + complex2)
    check(completions(complex1, complex2), List("orange"))

  }
}
