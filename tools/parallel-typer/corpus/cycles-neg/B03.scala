package cycles
object B03 {
  def g = A03.f
  val w = A03.v
  def chainMiddle = A03.chainEnd + 1
}
trait TB03 extends TA03 { def inh2 = inh }
