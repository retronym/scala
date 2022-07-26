// scalac: -Xasync -Xprint:parser,typer,async -Xprint-pos -Yrangepos

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
    testBasic()
  }

  private def testBasic() = optionally {
    val x = value(Some(1))
    val y = value(Some(2))
    x + y
  }
}