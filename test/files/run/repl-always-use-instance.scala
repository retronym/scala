import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |case class Name(value: String) // show
    |val x = Name("foo") // show
    |val y = Name("bar")
    |val z = Name(x.value + y.value) // show
    |""".stripMargin
}
