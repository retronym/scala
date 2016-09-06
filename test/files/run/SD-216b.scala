trait A[T] { def apply(x: T): T }
trait B extends A[Int] { def apply(x: Int): Int }

object Test {
  def f: B = (x: Int) => x + 1
  def g: A[Int] = f

  def main(args: Array[String]): Unit = {
    assert(f(10) == 11)
    assert(g(10) == 11)
  }
}
