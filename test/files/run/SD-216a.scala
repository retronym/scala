trait A[T] { def apply(x: T): T }
trait B extends A[String] { def apply(x: String): String }

object Test {
  def f: B = (x: String) => x + "!"
  def g: A[String] = f

  def main(args: Array[String]): Unit = {
    assert(f("hello") == "hello!")
    println(g("hello") == "hello!")
  }
}
