package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A04 {
  def f = B04.g
  val v = B04.w
  def chainStart = B04.chainMiddle
  def chainEnd: Int = 04
}
trait TA04 { def m: Int; def inh = new TB04 { def m = 1 }.inh2 }
