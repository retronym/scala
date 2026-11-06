package nameclash

// Overload resolution types the argument silently and fails with a mismatch between scala.Long and java.lang.Long,
// whose message is rendered with both names qualified. That must not disturb the other units, which only ask for Long.
class Use12 {
  def o(x: java.lang.Long): Int = 1
  def o(x: String): Int = 2
  def a(l: Long): Int = o(l)
  def b(n: Long, m: Long): Long = n + m
  def c(xs: List[Long]): Long = xs.sum
  val d: Long = 1L
  def e: Long => Long = (x: Long) => x
  def f(l: Long, m: Long = 2L): Int = o(l) + o(m)
}
