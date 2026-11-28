import scala.language.{existentials, higherKinds}

class Ref[T](var v: T)
trait K[F[_]] { def get: F[String]; def put(x: F[Any]): Unit }
trait Cov[+T]

object Test {
  def use[F[+X]](q: (F[Any], F[Any] => Unit)): Unit = q._2(q._1)

  def applied(p: (Ref[String], Ref[Any] => Unit)): Unit = {
    // needs a covariant F with Ref[String] <: F[Any] and F[Any] <: Ref[Any]
    val e: (F[Any], F[Any] => Unit) forSome { type F[+X] } = p    // error
    e match { case (a, g) => g(a) }
  }

  def unapplied(k: K[Ref], c: K[Cov], l: K[List]): Unit = {
    val e: K[F] forSome { type F[+X] } = k                        // error
    val ok1: K[F] forSome { type F[+X] } = c
    val ok2: K[F] forSome { type F[+X] } = l
    val ok3: K[F] forSome { type F[X] } = k
    val ok4: K[F] forSome { type F[X] } = c
  }

  def ok(p: (List[String], List[Any] => Unit), q: (Ref[Any], Ref[Any] => Unit)): Unit = {
    val e1: (F[Any], F[Any] => Unit) forSome { type F[+X] } = p
    val e2: (F[Any], F[Any] => Unit) forSome { type F[X] } = q
  }
}
