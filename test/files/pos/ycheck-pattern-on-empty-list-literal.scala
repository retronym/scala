//> using options -Ycheck:all -Werror

object Test {
  def m(): Long = List[Int]() match { case a :: tl => 5L; case x => x.length.toLong }
}
