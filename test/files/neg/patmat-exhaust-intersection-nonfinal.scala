//> using options -Werror
sealed trait T
trait M
case class AM() extends T with M
case class B() extends T
class C extends T

sealed trait U
case class DM() extends U with M
final case class F() extends U
case object G extends U

sealed trait V { type A }
class D extends V { type A = Int }
class E extends V { type A = String }

sealed trait W
case class WM() extends W with M

object Test {
  // Not exhaustive: neither B nor C is final, so `new B with M` and `new C with M` inhabit T with M.
  def f1(x: T with M): Int = x match { case AM() => 1 }
  // The second case is reachable, `f2(new B with M)` is 2. Not exhaustive for `new C with M`.
  def f2(x: T with M): Int = x match { case _: AM => 1; case _: B => 2 }
  // Exhaustive: a final class cannot be extended with M, and an object has no subclass.
  def f3(x: U with M): Int = x match { case DM() => 1 }
  // Exhaustive: no subclass of E can redefine A.
  def f4(x: V { type A = Int }): Int = x match { case _: D => 1 }
  // Exhaustive: Local has no subclass and none can be written outside the block.
  def loc: W = { class Local extends W; new Local }
  def f5(x: W with M): Int = x match { case WM() => 1 }
}
