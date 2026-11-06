package cycles
object B06 {
  def g = A06.f
  val w = A06.v
  def chainMiddle = A06.chainEnd + 1
}
trait TB06 extends TA06 { def inh2 = inh }
