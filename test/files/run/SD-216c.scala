trait A[T] { def apply(x: T): T }

object Test {
  def f: A[String] = (x: String) => x + "!"

  def main(args: Array[String]): Unit = {
    assert(f("hello") == "hello!")
    assert(f.getClass.getName.contains("Lambda"), f.getClass)
  }
}
