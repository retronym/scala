object Test {
  def m(): Long = List[Int]() match { case a :: tl => 5L; case x => x.length.toLong }
  def main(args: Array[String]): Unit = println(m())
}
