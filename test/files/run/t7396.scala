class Value(val x: Any) extends AnyVal
object Test extends App {
  println(new Value(null).toString)
  println((new Value(null): Any).toString)
  println(new Value(null).hashCode)
  println((new Value(null): Any).hashCode)
}
