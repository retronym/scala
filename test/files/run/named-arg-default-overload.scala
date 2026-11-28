object O {
  def f(a: Object): String = "f(Object)"
  def f(a: String, b: Int = 0): String = "f(String, Int = 0)"

  def h(a: Any): String = "h(Any)"
  def h(a: Int, b: Int = 0): String = "h(Int, Int = 0)"
}

// Naming the argument should not change the alternative: one that needs
// a default argument is only considered if no other alternative applies.
object Test {
  def main(args: Array[String]): Unit = {
    println(O.f("s"))
    println(O.f(a = "s"))
    println(O.h(1))
    println(O.h(a = 1))
  }
}
