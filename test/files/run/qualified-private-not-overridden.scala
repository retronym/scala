// A `private[A]` member of class A is name-expanded in SuperAccessors, before RefChecks, so a
// member of the same name in a subclass is neither an override (no `override` needed, no
// override checks) nor seen by A's own callers. SLS 5.2 does not say that qualified private
// members are excluded from inheritance. Scala 3 requires `override` on `B.f`.
// Pinned as on 2.13.x.
//
// TODO scala/scala review H56: if `private[A]` members are made overridable as in Scala 3, `B.f`
// needs `override` and `b.callF` prints 2; if a diagnostic is added instead, this file compiles
// with a warning at `B.f`, which appears in the .check.
class A { private[A] def f = 1; def callF = f }
class B extends A { def f = 2 }

object Test {
  def main(args: Array[String]): Unit = {
    val b = new B
    println(b.callF)
    println(b.f)
  }
}
