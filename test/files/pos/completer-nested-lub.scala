// A completer forced in the middle of a type operation typechecks with its own recursion guards.
// Here, `lub(B, C)` forces `B.f`, and inferring `T` in its body must not find the guard of that lub.
trait P { def f: Any }
object Test { def g(b: Boolean) = if (b) new B else new C }
class B extends P { def f = { val x = H.pick(new C, new B); x } }
class C extends P { def f = { val x = H.pick(new C, new B); x } }
object H { def pick[T](x: T, y: T): T = x }
object Check {
  val b: P = (new B).f
  val c: P = (new C).f
}
