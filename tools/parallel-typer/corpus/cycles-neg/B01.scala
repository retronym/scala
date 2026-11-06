package cycles
object B01 {
  def g = A01.f
  val w = A01.v
  def chainMiddle = A01.chainEnd + 1
}
trait TB01 extends TA01 { def inh2 = inh }
