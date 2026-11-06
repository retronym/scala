package cycles
object B05 {
  def g = A05.f
  val w = A05.v
  def chainMiddle = A05.chainEnd + 1
}
trait TB05 extends TA05 { def inh2 = inh }
