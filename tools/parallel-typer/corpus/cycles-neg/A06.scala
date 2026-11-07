package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A06 {
  def f = B06.g
  val v = B06.w
  def chainStart = B06.chainMiddle
  def chainEnd: Int = 06
}
trait TA06 { def m: Int; def inh = new TB06 { def m = 1 }.inh2 }
