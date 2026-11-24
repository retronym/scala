// s and raw take Any*: on a StringContext that is not a literal, each argument keeps its own type

object Test extends App {
  def check(expected: String, actual: String): Unit = assert(expected == actual, s"expected $expected, got $actual")

  val sc = StringContext("", " ", "")
  check("1 1.5", sc.s(1, 1.5))
  check("a 1", sc.s('a', 1))
  check("0.1 1.0", sc.raw(0.1f, 1.0))
  check("1 1.5", new StringContext("", " ", "").s(1, 1.5))
  check("1 1.5", s"${1} ${1.5}")
  check("1 1.5", sc.s(Seq[Any](1, 1.5): _*))
  check("1 2", sc.s(Seq(1, 2): _*))
}
