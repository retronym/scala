object Test extends App {

  val vector1 = Vector.fill(1000)(0)

  // Some prepending, manifesting the bug seems to require 2 separate prepends
  val vector2 = List.fill(25)(1) ++: 2 +: vector1

  // Now append for fun and profit
  val vector3 = vector2 ++ List.fill(40)(3)

  // Any iteration will do although might hit NullPointerException in different place
  val vector4 = vector3.collect { case n => n }
  assert(vector3 == vector4)  
}
