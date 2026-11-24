// `null` is not compatible with a by-name parameter whose underlying type is a value type
object Test {
  def f(x: Any): String = "Any"; def f(x: => Unit): String = "byname-unit"
  def h(x: Any): String = "Any"; def h(x: => Int): String = "byname-int"
  def k(x: => Int): String = "byname-int"; def k(x: => String)(implicit d: DummyImplicit): String = "byname-string"
  def r(x: Any): String = "Any"; def r(x: => String): String = "byname-string"

  def main(args: Array[String]): Unit = {
    println(f(null))
    println(f(()))
    println(h(null))
    println(h(1))
    println(k(null))
    println(k(1))
    println(r(null))
    println(r(1))
  }
}
