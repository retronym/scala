package cycles
// genuine cycles through inferred result types, across units, and chains that are not cycles
object A01 {
  def f = B01.g
  val v = B01.w
  def chainStart = B01.chainMiddle
  def chainEnd: Int = 01
}
trait TA01 { def m: Int; def inh = new TB01 { def m = 1 }.inh2 }
