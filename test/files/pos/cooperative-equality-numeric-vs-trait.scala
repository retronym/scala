//> using options -Werror

// A number or character compared with a value of a trait type is compared cooperatively, so it is
// not "always false": the value can be a ScalaNumber.
class C {
  trait T
  def f(o: Ordered[BigInt], t: T, s: Iterable[Int]) =
    List(1 == o, 'a' == o, 1L != o, 1 == t, (1: Integer) == t, 1 == s, o == 1, t == 'a')
}
