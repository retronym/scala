import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test {
  val toolbox = cm.mkToolBox(options = "-Yrangepos")

  def main(args: Array[String]) {
    def test(expr: String) {
      val t = toolbox.parse(expr)
      println(expr)
      println(show(t, printPositions = true))
      // println(showRaw(t, printPositions = true))
      t match {
        case imp: Import =>
          println(show(imp.expr, printPositions = true))
        case _ =>
      }
      println()
    }
    val tests = """
    val x = 0
    var x = 0
    val x, y = 0
    var x, y = 0
    val Pat(x) = 0
    val (a, b) = null
    def x = 0
    import a.b
    import a.{b => c, d}
    import a.b, c.d
    new C
    new C[A] {}
    new Some("")
    def foo[A: T] = 0
    """
    val exprs = tests.split("\\n").map(_.trim).filterNot(_.isEmpty)
    exprs foreach test
    // test("val (a, b) = (0, 1)")
  }
}
