//> using options -opt:local

// Box-unbox elimination must not treat boxes that flow around a cycle of local variable copies as a single creation
object Test {
  def f(n: Int, a: Long, b: Int, c: Int): Int = {
    var x: Any = a; var y: Any = b; var z: Any = c
    var i = 0; var s = 0
    while (i < n) {
      val t = x; x = y
      if (i % 3 != 2) s = s * 100 + x.asInstanceOf[Int]
      y = z; z = t; i += 1
    }
    s * 100 + y.asInstanceOf[Long].toInt
  }
  def main(args: Array[String]): Unit = println(List(f(2, 5L, 7, 9), f(5, 5L, 7, 9)))
}
