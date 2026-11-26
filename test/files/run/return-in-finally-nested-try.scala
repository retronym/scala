//> using options -nowarn
class B1 {
  // return in the finally of a try with a non-Unit value, inside another try-finally
  def f(a: Int): Int =
    try {
      try { a } finally { if (a > 0) return 6 }
    } finally { println("outer finally") }
  // the same with handlers, and a value that is not the method's result
  def g(a: Int): String = {
    val x =
      try {
        try { a } catch { case _: Exception => -1 } finally { if (a > 0) return "returned" }
      } catch { case _: Error => -2 } finally { println("outer finally") }
    "fell through " + x
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    try { println(new B1().f(1)); println(new B1().f(0)); println(new B1().g(1)); println(new B1().g(0)) }
    catch { case e: VerifyError => println("VerifyError: " + e.getMessage.linesIterator.next()) }
  }
}
