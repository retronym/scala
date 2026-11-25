//> using options -opt:inline:**
final class C { @inline def m(): Int = 42 }
object Test {
  def main(args: Array[String]): Unit = {
    // c is null: c.m() must throw NullPointerException
    val f = (c: C, s: String) => { val r = c.m(); println(s + r); r }
    val g: (C, String) => Int = if (args.length == 0) f else null
    try println(g(null, "result: ")) catch { case e: NullPointerException => println("NPE") }
  }
}
