// scalac: -language:higherKinds
//
object Test {
  case class Foo[CC[_], D <: CC[Int]](d: D, cc: CC[Int])
  Foo(Nil, List(1, 2, 3))

  class H[F]
  def g[F, T, FT <: Set[(F, T)]](h: H[F]) = 1
  g(new H[Set[(Int, String)]])
}
