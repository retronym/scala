package cycles
object B02 {
  def g = A02.f
  val w = A02.v
  def chainMiddle = A02.chainEnd + 1
}
trait TB02 extends TA02 { def inh2 = inh }
