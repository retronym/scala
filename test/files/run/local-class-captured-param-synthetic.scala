object Test {
  def make(k: Int): AnyRef = {
    class Local(xs: List[String]) { def size = xs.size + k }
    new Local(List("a"))
  }
  def main(args: Array[String]): Unit = {
    val c = make(1).getClass.getDeclaredConstructors.head
    println(c.getParameterTypes.map(_.getName).mkString("descriptor: ", ", ", ""))
    println(c.getGenericParameterTypes.mkString("generic: ", ", ", ""))
    try println(c.getParameters.map(_.getParameterizedType).mkString("parameters: ", ", ", ""))
    catch { case e: Throwable => println(e) }
  }
}
