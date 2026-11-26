case class Meters(value: Double) extends AnyVal
object M2 { // collect whose case body is a match on types
  def main(a: Array[String]): Unit = println(List[Any](1.5, "2.5", "x").collect { case x => x match { case d: Double => Meters(d); case s: String => try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) } } })
}
