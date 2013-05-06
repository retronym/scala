import scala.util.continuations._

object Test {
  def main_loop: Nothing = reset {
      shift { k: (Unit => Nothing) => k() }
      main_loop
  }

  def main(argv: Array[String]) = {main_loop}
}
