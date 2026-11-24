import scala.annotation.varargs

abstract class A {
  type T
  type U <: AnyRef
  type V <: AnyVal
  type W >: Null
  @varargs def unbounded(xs: T*): Int = xs.length
  @varargs def refBounded(xs: U*): Int = xs.length
  @varargs def valBounded(xs: V*): Int = xs.length
  @varargs def lowerBounded(xs: W*): Int = xs.length
  @varargs def last(x: T, xs: T*): T = (x +: xs).last
}

class Strings extends A {
  type T = String
  type U = String
  type V = Int
  type W = String
}

class Ints extends A {
  type T = Int
  type U = String
  type V = Int
  type W = String
}

class Outer[X] {
  type Alias = X
  @varargs def alias(xs: Alias*): Int = xs.length
}
