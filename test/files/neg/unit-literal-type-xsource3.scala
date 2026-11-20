//> using options -Xsource:3
class C[A, B] {
  def typeArg: Either[String, ()] = ???
  def typeArg1: Option[()] = ???
  def tupleElem: (Int, ()) = ???
  def infixOperand: Int C () = ???
  def repeated(xs: ()*) = ()
  def byName(x: => ()) = ()
  def bound[X: ()] = ()
  def upper[X <: ()] = ()
  def compound(x: AnyRef with ()) = ()
  def instance = new ()
}
