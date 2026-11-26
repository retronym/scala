//> using options -opt:local

// As value-class-box-try, under the optimizer, whose analysis rejected the method.

case class Meters(value: Double) extends AnyVal

object Test {
  def meters(s: String): Meters = Meters(s.toDouble)

  def collect(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case s: String => try meters(s) catch { case _: NumberFormatException => Meters(0) }
  } }

  def ascribed(s: String): Any = (try meters(s) catch { case _: NumberFormatException => Meters(0) }): Meters

  def main(args: Array[String]): Unit = {
    println(collect(List(1.5, "2.5", "x")))
    println(List("2.5", "x").map(ascribed))
  }
}
