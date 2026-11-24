// A `private[A]` member is not overridden (run/qualified-private-not-overridden), but the namer
// still takes it as the overridden member when it infers the result type of `B.f`, so the
// inferred type is A's Int and "x" does not conform. With `def f: String` this compiles.
// Pinned as on 2.13.x.
//
// TODO scala/scala review H56: if `private[A]` members stay non-overridable, the namer should not
// use them for inference and this file moves to pos/; if they become overridable as in Scala 3,
// the error here is an override error instead.
class A { private[A] def f = 1 }
class B extends A { def f = "x" }
