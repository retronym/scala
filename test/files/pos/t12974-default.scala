// Without -Xsource:3, `getClass` on a non-public class is typed `Class[_]`, so the private
// class does not escape. neg/t12974-escape is the same code under -Xsource:3, where it is an
// error since scala/scala#11302.
//
// TODO scala/scala review Y1: if the precise type is moved behind a -Xsource-features choice,
// this file is unchanged and neg/t12974-escape gains that choice in its `using options` line
// (plain -Xsource:3 would then compile it, as 2.13.18 does).
trait C {
  private trait Style
  private object P

  def a() = P.getClass
  def b(s: AnyRef) = { val c = s.asInstanceOf[Style].getClass; c }

  private def foo(s: Style) = s.getClass
  def c(s: AnyRef) = foo(s.asInstanceOf[Style])
}
