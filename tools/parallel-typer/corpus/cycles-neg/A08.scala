package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A08 {
  def f = B08.g
  val v = B08.w
  def chainStart = B08.chainMiddle
  def chainEnd: Int = 08
}
trait TA08 { def m: Int; def inh = new TB08 { def m = 1 }.inh2 }
