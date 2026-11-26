case class Meters(value: Double) extends AnyVal
object Ord {
  def parse(x: Any): Any = (x match {
    case d: Double => Meters(d)
    case s: String => try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) }
  }): Meters
  def parseAll(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case s: String => try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) }
  } }
  def main(a: Array[String]): Unit = {
    println(scala.util.Try(parseAll(List(1.5, "2.5", "x"))))
    println(scala.util.Try(parse("x")))
  }
}
