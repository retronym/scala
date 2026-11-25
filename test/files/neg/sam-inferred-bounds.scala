trait F[T <: String] {
  def apply(x: Int): T
  def len: Int = apply(0).length
}
trait G[T >: Null <: AnyRef] { def apply(x: Int): Int }
trait H[T <: String] { def apply(x: T): Int }

object Test {
  val f1: F[_] = (x: Int) => 42               // error: F[Int] is not within the bound of T
  val f2: F[_ <: AnyRef] = (x: Int) => List(x) // error
  val f3: F[_] = (x: Int) => "s"
  val f4: F[_ <: String] = (x: Int) => "s"
  val g1: G[_] = (x: Int) => x                // T does not occur in the method
  val h1: H[_] = (x: String) => x.length
  val h2: H[_] = (x: Any) => 0
}
