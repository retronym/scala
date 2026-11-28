package u
trait Fun[F[_]] { def map[A, B](fa: F[A])(f: A => B): F[B]; def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ()); def unit: F[Unit] = ??? }
abstract class Base[X] { def x: X = ??? }
object Make {
  def make[A0]: Fun[({type L[a] = (A0, a)})#L] = new Base[A0] with Fun[({type L[a] = (A0, a)})#L] {
    def map[A, B](fa: (A0, A))(f: A => B) = (fa._1, f(fa._2))
  }
}
