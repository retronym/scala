package pneg

// The scanner has a token in hand when the XML literal ends: whether that is an error depends on the errors of the
// units before this one (`Scanner.resume`).
object A05XmlCase {
  def proc() { }
  val c = <d>{ 1
    case }</d>
  val after = 'sym
}
