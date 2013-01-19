trait A {
  sealed abstract class X
  private class X1 extends X with X2 { }
  private trait X2 extends X
  sealed trait X3 extends X

  def f(x: X) = x match { // was: It would fail on the following input: (_ : A.this.X2)
    case _: X1 => 0
  }
}
