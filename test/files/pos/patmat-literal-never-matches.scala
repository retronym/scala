//> using options -Werror -Xlint

// Numeric literal patterns that no value of the expected type can equal are accepted without a
// diagnostic; SLS 8.1.4 requires a literal pattern's type to conform to the expected type.
// Pinned as on 2.13.x (scala/bug#12722 was closed as not planned).
//
// TODO scala/scala review S1: if a warning is added, this test moves to neg/ with one warning
// per method below; if it becomes an error under -Xsource:3 (as scala/scala#10290 proposed),
// a neg/ copy with -Xsource:3 gets one error per method.
object Test {
  def f(x: Int) = x match { case 1.5 => "a"; case _ => "b" }     // Double literal on an Int
  def g(x: Byte) = x match { case 1000 => "a"; case _ => "b" }    // out of range for Byte
  def h(x: Char) = x match { case 70000 => "a"; case _ => "b" }   // out of range for Char
}
