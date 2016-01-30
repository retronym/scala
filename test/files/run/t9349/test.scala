object Test {
  def main(args: Array[String]): Unit = {
    val o1 = Outer(5)
    o1 match {
      case o @ Outer(_) =>
        val i = new o.Inner
    }
  }
}
