object Test {
  def main(args: Array[String]): Unit = {
    // A varargs method must have an array as its last parameter, otherwise javac rejects the class file.
    for (m <- classOf[A].getDeclaredMethods.filter(_.isVarArgs).sortBy(_.getName))
      println(m.toGenericString)
    J.run()
  }
}
