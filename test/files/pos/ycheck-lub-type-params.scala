//> using options -Ycheck:typer,uncurry -Werror

// The inferred result types are refinements whose members are polymorphic.
// Their type parameters are bound in the type, not out of scope.
object T {
  def f(b: Boolean) = if (b) List(1) else Vector(1)
  def g(b: Boolean) = (x: Int) => if (b) List(x) else Vector(x)
}
