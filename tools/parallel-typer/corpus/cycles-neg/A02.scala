package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A02 {
  def f = B02.g
  val v = B02.w
  def chainStart = B02.chainMiddle
  def chainEnd: Int = 02
}
trait TA02 { def m: Int; def inh = new TB02 { def m = 1 }.inh2 }
