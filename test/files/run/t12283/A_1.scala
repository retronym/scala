class Poly[A](val value: A) extends AnyVal
class Arr[A](val value: Array[A]) extends AnyVal
class Mono(val value: Int) extends AnyVal
class A {
  def poly1(x: Poly[Int]): Poly[Int] = x
  def arr1(x: Arr[Int]): Arr[Int] = x
  def polyMono(x: Poly[Mono]): Poly[Mono] = x
  def polyPoly(x: Poly[Poly[Int]]): Poly[Poly[Int]] = x
  def polyList(x: Poly[List[String]]): Poly[List[String]] = x
}
