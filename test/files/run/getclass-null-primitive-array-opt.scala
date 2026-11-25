//> using options -opt:local
object Test {
  def f(a: Array[Int]): Class[_] = a.getClass
  def g(a: Array[Array[Double]]): Class[_] = a.getClass
  def len(a: Array[Int]): Int = java.lang.reflect.Array.getLength(a)
  def tr(body: => Any): Unit = try println(body) catch { case e: Throwable => println("EXC " + e.getClass.getName) }
  def main(args: Array[String]): Unit = {
    tr(f(Array(1))); tr(f(null)); tr(g(null)); tr(len(null))
  }
}
