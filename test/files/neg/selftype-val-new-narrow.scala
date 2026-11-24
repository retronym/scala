// Only the right-hand side of the definition of a value that cannot be overridden
// is narrowed to the value's singleton type.
object O {
  class C { this: O.x.type => }
  val x: C = new C
  val y: C = new C              // error: y.type is not x.type
}

object L {
  class D { this: D with Singleton => }
  var v: D = new D              // error: a var is not stable
  def m = new D                 // error: not a value definition
}

// If `x` were narrowed here, the instance created in A's initializer would be
// typed as `x.type` although `B` overrides `x` with another instance.
class A {
  var stash: List[x.type] = Nil
  class C { self: A.this.x.type =>
    stash ::= self
  }
  val x: C = new C              // error: x can be overridden
}
class B extends A {
  override val x: C = new C     // error
}
final class F {
  class C { this: F.this.x.type => }
  val x: C = new C
}
