object Test {
  def arg(x: Int): Int = { println(s"arg $x"); x }

  def check(body: => Any): Unit =
    try { body; println("no exception") }
    catch { case _: NullPointerException => println("NullPointerException") }

  def main(args: Array[String]): Unit = {
    val n: J = null
    // JLS 15.9.4: a null enclosing instance throws before the arguments are evaluated
    check(new n.JI(arg(1)))
    check(new n.JI(arg(2)) { })
    class S extends n.JI(arg(3))
    check(new S)

    val j = new J
    check(new j.JI(arg(4)))
    class T extends j.JI(arg(5))
    check(new T)
  }
}
