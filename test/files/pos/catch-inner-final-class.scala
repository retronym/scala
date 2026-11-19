//> using options -Werror
// The outer pointer of a final inner class may be elided, so a type test for it has no outer test.
// A catch for such a class uses the exception table, without an "outer reference" warning.
class Top {
  final class E extends Exception

  def multi(t: => Unit): String =
    try { t; "" } catch {
      case _: E         => "mine"
      case _: Exception => "other"
    }

  def bound(t: => Unit): String =
    try { t; "" } catch {
      case e: E         => e.getMessage
      case e: Exception => "other"
    }

  def path(other: Top)(t: => Unit): String =
    try { t; "" } catch {
      case _: other.E   => "theirs"
      case _: Exception => "other"
    }
}
