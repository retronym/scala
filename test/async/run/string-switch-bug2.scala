//> using options -Xasync -Vprint:async
import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

// Scala.js compatible test suite for -Xasync that doesn't use Scala futures
object Test {
  def main(args: Array[String]): Unit = {
    stringSwitchBug()
  }
  def log[A](x: A): A = x

  private def stringSwitchBug() = {
    assertEquals(Some(true), optionally {
      val x = toString match {
        case "a" => log("A")
        case "b" => log("B")
        case "c" if value(Some("")).toString == "" => log("c")
        case _ =>
          log(("_", value(Some("_"))))
      }
      x == ("_", "_")
    })
  }
}
