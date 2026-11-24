class Outer[C] {
  trait Inner[A] { val x: Int = 1 }
}
object Test {
  def main(args: Array[String]): Unit = {
    val inner = Class.forName("Outer$Inner")
    val m = inner.getMethod("$init$", inner)
    println(m)
    try println(m.toGenericString)
    catch { case e: Throwable => println(e) }
  }
}
