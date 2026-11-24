//> using options -opt:local
object Test {
  def f(c: Boolean, e1: Throwable, e2: Throwable): String = {
    val e = if (c) e1 else e2
    try { throw e } catch { case t: Throwable => "caught " + t.getMessage }
  }
  def main(args: Array[String]): Unit = {
    try println(f(true, new Exception("one"), new Exception("two"))) catch { case t: Throwable => println("escaped " + t.getMessage) }
    try println(f(false, new Exception("one"), new Exception("two"))) catch { case t: Throwable => println("escaped " + t.getMessage) }
  }
}
