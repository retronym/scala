case class Meters(value: Double) extends AnyVal
object Ua {
  def a(s: String): Any = (try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) }): Meters
  def main(x: Array[String]): Unit = println(a("x"))
}
