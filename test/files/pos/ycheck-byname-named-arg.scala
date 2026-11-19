//> using options -Ycheck:_ -Werror

// A by-name argument passed by name out of order is stored in a temporary of type `() => T`.
// For a constant argument, `T` was the constant type, which the function that uncurry
// creates from the lifted body (of the widened type) did not conform to.
object Test {
  def f(a: => Int, b: => Int): Int = a + b
  def g = f(b = { println("b"); 1 }, a = 2)
}
