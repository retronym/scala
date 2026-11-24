object Test {
  def show(f: => Any): Unit = println(try String.valueOf(f) catch { case t: Throwable => t.getClass.getName })
  def main(args: Array[String]): Unit = {
    val x: Any = "s"
    show { x.asInstanceOf[Array[_]]; "no exception" }
    show { val arr: Array[_] = x.asInstanceOf[Array[_]]; (arr: Any).getClass }
    show(x.asInstanceOf[Array[_]].length)
    show(x.asInstanceOf[Array[_]](0))
    show { def cast[T](y: Any): Array[T] = y.asInstanceOf[Array[T]]; cast[Int](x).length }
    show { x.asInstanceOf[Array[String]]; "no exception" }
  }
}
