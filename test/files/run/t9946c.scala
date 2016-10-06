class Test(private[this] var x: String) {
  // lazy val y = x.reverse
  def xx = x
  def set = x = ""
  def inner = new { assert(x != null) }
}
// object Test {
//   def main(args: Array[String]): Unit = {
//     val t = new Test("foo")
//     assert(t.y == "oof", t.y)
//     t.inner
//   }
// }

