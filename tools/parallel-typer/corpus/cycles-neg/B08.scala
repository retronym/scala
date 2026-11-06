package cycles
object B08 {
  def g = A08.f
  val w = A08.v
  def chainMiddle = A08.chainEnd + 1
}
trait TB08 extends TA08 { def inh2 = inh }
