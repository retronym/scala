package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A03 {
  def f = B03.g
  val v = B03.w
  def chainStart = B03.chainMiddle
  def chainEnd: Int = 03
}
trait TA03 { def m: Int; def inh = new TB03 { def m = 1 }.inh2 }
