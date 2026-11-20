object App {
  def main(args: Array[String]): Unit = println(Macros.evalConst(1 + 2))
}
