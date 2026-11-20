// A pattern definition with several variables, one of them ascribed a singleton type.
object Test {
  val f: (1, "a") = (1, "a")
  val (a: 1, b) = f
  val s = "x"
  val g: (s.type, Int) = (s, 2)
  val (c: s.type, d) = g
  val (Some(e: 1), h: "a") = (Some(1), "a"): @unchecked

  def local = {
    val (p: 1, q) = f
    (p, q)
  }

  def main(args: Array[String]): Unit = {
    val a1: 1 = a
    val c1: s.type = c
    println((a1, b, c1, d, e, h, local))
    for ((x: 1, y) <- List(f)) println((x: 1, y))
    try {
      val (x: 1, y) = ((2, 3): Any): @unchecked
      println(x)
    } catch { case e: MatchError => println(e.getMessage) }
  }
}
