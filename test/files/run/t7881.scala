object Test {
  def f = {
    val a = 100
    ({ val a = 0; (c: Int) => c })(a)
  }
  def g = {
    val x = "a"
    ({ val x = "b"; (s: String) => s })(x)
  }
  def main(args: Array[String]): Unit = {
    println(f)
    println(g)
  }
}
