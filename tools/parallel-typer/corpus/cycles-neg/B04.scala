package cycles
object B04 {
  def g = A04.f
  val w = A04.v
  def chainMiddle = A04.chainEnd + 1
}
trait TB04 extends TA04 { def inh2 = inh }
