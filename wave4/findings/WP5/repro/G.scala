object G {
  def f(d: Int): Int = d * 10
  def loc(b: Boolean, c: Boolean): Int = {
    try {
      if (b) return 2
      f(if (c) return 4 else 1)
    } finally println("fin")
  }
  def main(args: Array[String]): Unit = println(List(loc(true, false), loc(false, true), loc(false, false)))
}
