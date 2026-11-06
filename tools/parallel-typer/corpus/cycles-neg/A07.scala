package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A07 {
  def f = B07.g
  val v = B07.w
  def chainStart = B07.chainMiddle
  def chainEnd: Int = 07
}
trait TA07 { def m: Int; def inh = new TB07 { def m = 1 }.inh2 }
