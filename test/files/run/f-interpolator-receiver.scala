// f is a member of StringContext: the receiver is evaluated and its parts are formatted,
// as for s, even when the receiver is not StringContext.apply on literals.
object Test {
  def mk(parts: String*): StringContext = {
    println("mk called")
    StringContext(("[" + parts.head) +: parts.tail: _*)
  }
  class Bracket(ps: String*) extends StringContext(("[" + ps.head) +: ps.tail: _*)

  def main(args: Array[String]): Unit = {
    println(mk("a", "%d b").f(1))
    println(mk("a", " b").s(1))
    println(new Bracket("a", "%d b").f(1))
    println(new Bracket("a", " b").s(1))
  }
}
