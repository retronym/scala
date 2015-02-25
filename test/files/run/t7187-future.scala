object Test {
  def foo(): () => String = () => ""
  val f: () => Any = foo

  def main(args: Array[String]): Unit = {
    println(f()) // currently, <function0>, would be: "foo"
  }
}
