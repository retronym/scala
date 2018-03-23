class C {
  println("effect")
  final val const = 42
}

object Test {
  def main(args: Array[String]) {
    val x = new C().const
  }
}