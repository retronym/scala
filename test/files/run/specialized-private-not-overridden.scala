class A[@specialized(Int) T](t: T) {
  private def show(x: T): String = "A.show(" + x + ")"
  def describe: String = show(t)
}
class B extends A[Int](1) {
  def show(x: Int): String = "B.show(" + x + ")"   // unrelated to A's private method
}

class Box[@specialized(Int) T](t: T) {
  private val v: T = t
  def get: T = v
}
class IntBox extends Box[Int](1) {
  def v: Int = 99                                   // unrelated to Box's private val
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new A[Int](1).describe)
    println(new B().describe)
    println(new IntBox().get)
  }
}
