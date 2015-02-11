class Test {
  def apply = pack.O.foo("")
}

object Test {
  def main(args: Array[String]): Unit = {
    new Test().apply  
  }
}