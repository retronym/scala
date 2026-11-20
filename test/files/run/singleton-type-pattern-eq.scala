// SLS 8.2 says a singleton type pattern `p.type` compares with `eq`. For a path that is not an
// AnyRef, `case _: x.type` compares with `==` (scala/bug#4577). On 2.13.x `isInstanceOf[x.type]`
// calls `equals` on the box, so the two disagree for a box of another class, such as 1000L.
//
// scala/scala review C6: on this branch retronym/scala#426 is merged, which makes `isInstanceOf`
// use `==` as the pattern does, so the two now agree and the `isInstanceOf 1000 / 1000L` line
// prints `true` like the `case` line above it.
object Test {
  def viaCase(x: Int, a: Any)       = a match { case _: x.type => true; case _ => false }
  def viaIsInstance(x: Int, a: Any) = a.isInstanceOf[x.type]

  def main(args: Array[String]): Unit = {
    println(s"case 1000 / 1000L:         ${viaCase(1000, 1000L)}")
    println(s"isInstanceOf 1000 / 1000L: ${viaIsInstance(1000, 1000L)}")
    println(s"case 1000 / new box:         ${viaCase(1000, Integer.valueOf(1000))}")
    println(s"isInstanceOf 1000 / new box: ${viaIsInstance(1000, Integer.valueOf(1000))}")
  }
}
