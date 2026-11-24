object Test {
  def kind(x: Any): String = x match {
    case _: Array[AnyVal] => "Array[AnyVal]"
    case _                => "other"
  }
  def main(args: Array[String]): Unit = {
    println(kind(Array("a")))
    println(kind(Array(1)))
    println((Array("a"): Any).isInstanceOf[Array[AnyVal]])
    (Array("a"): Any) match {
      case vs: Array[AnyVal] => val v: AnyVal = vs(0); println("AnyVal element of " + (v: Any).getClass)
      case _                 => println("other")
    }
  }
}
