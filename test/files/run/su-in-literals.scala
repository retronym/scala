// The U+001A characters in this file are literal: one is only the end of the input at the end of the input.
object Test {
  // line  comment
  /* block  comment */
  /** doc  comment */
  val str    = "ab"
  val multi  = """ab"""
  val interp = s"ab${str.length}"
  val interpMulti = s"""ab${str.length}"""
  val chr    = ''

  def main(args: Array[String]): Unit = {
    def codes(s: String) = s.map(_.toInt).mkString(" ")
    println(codes(str))
    println(codes(multi))
    println(codes(interp))
    println(codes(interpMulti))
    println(chr.toInt)
  }
}
