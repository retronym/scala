object Test {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 2)
      println('foo) // Set a breakpoint in Symbol#apply, it will only be called once.
  }
}
