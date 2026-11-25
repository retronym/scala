import scala.language.reflectiveCalls
object Test {
  class C { def m(x: Int): Int = x }
  def arg(): Int = { println("arg evaluated"); 1 }
  def main(a: Array[String]): Unit = {
    val s: { def m(x: Int): Int } = null
    try s.m(arg()) catch { case e: NullPointerException => println("NPE structural") }
    val c: C = null
    try c.m(arg()) catch { case e: NullPointerException => println("NPE nominal") }
  }
}
