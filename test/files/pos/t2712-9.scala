trait Iterable[+A]
trait Map[A, B] extends Iterable[(A, B)]

trait Other[+A]
trait Map1[A, B] extends Other[(A, B)] with Iterable[(A, B)]

class Test {
  def foo[M[+A] <: Iterable[A]](m: M[Any]): M[Any] = ???
  def test(m: Map[Int, String]) = {
    foo(m) // fails under -Yhigher-order-unification
  }

  def test1(m: Map1[Int, String]) = {
    foo(m) // fails in 2.11.8
  }
}
