trait A {
  def foo: Long
}

object Test {
  def a(): A = new A {
    var foo: Long = 1024L

    val test = () => {
      foo = 28
    }
  }
  def main(args: Array[String]) = assert(a().foo == 1024L)
}
