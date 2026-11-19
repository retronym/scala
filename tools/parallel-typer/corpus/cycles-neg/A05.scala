package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A05 {
  def f = B05.g
  val v = B05.w
  def chainStart = B05.chainMiddle
  def chainEnd: Int = 05
}
trait TA05 { def m: Int; def inh = new TB05 { def m = 1 }.inh2 }
