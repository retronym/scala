// A recursive call in a method of a generic value class, on a qualifier whose type does not
// supply the type arguments of the class.

import scala.annotation.tailrec
import scala.language.existentials

class V[T](val t: T) extends AnyVal {
  def f(o: V[_], n: Int): Int = if (n == 0) 0 else 1 + o.f(o, n - 1)
  def tail(o: V[_], n: Int, acc: Int): Int = if (n == 0) acc else o.tail(this, n - 1, acc + 1)
  def poly[A](o: V[_], as: List[A]): List[A] = if (as.isEmpty) Nil else as.head :: o.poly[A](o, as.tail)
  def bounded[O <: V[_]](o: O, n: Int): Int = if (n == 0) 0 else 1 + o.bounded(o, n - 1)
  def ownArgs(o: V[T], n: Int): Int = if (n == 0) 0 else 1 + o.ownArgs(o, n - 1)
  def nested(o: V[List[X]] forSome { type X }, n: Int): Int = if (n == 0) 0 else 1 + o.nested(o, n - 1)
}

class W[A, B](val a: A) extends AnyVal {
  def f(o: W[_, _], n: Int): Int = if (n == 0) 0 else 1 + o.f(o, n - 1)
  def half(o: W[A, _], n: Int): Int = if (n == 0) 0 else 1 + o.half(o, n - 1)
}

// With a variant type parameter, the qualifier conforms to the class instantiated at a bound of the
// quantified variable, `Cov[Any]` or `Contra[Nothing]`, so the call is rewritten and is a tail call.
class Cov[+T](val t: T) extends AnyVal {
  @tailrec final def f(o: Cov[_], n: Int): Int = if (n == 0) 0 else o.f(o, n - 1)
  @tailrec final def bounded(o: Cov[_ <: CharSequence], n: Int): Int = if (n == 0) o.t.length else o.bounded(o, n - 1)
  // an argument that mentions a quantified variable is taken as `List[_]`
  @tailrec final def nested(o: Cov[List[X]] forSome { type X }, n: Int): Int = if (n == 0) o.t.length else o.nested(o, n - 1)
}

class Contra[-T](val t: Any) extends AnyVal {
  @tailrec final def f(o: Contra[_], n: Int): Int = if (n == 0) 0 else o.f(o, n - 1)
}

class HK[M[_]](val v: Int) extends AnyVal {
  def f(o: HK[F] forSome { type F[_] }, n: Int): Int = if (n == 0) v else 1 + o.f(o, n - 1)
}

object Test extends App {
  val v = new V(1)
  println(v.f(new V(""), 3))
  println(v.tail(new V(""), 3, 0))
  println(v.poly(new V(""), List(1, 2, 3)))
  println(v.bounded(new V(""), 3))
  println(v.ownArgs(v, 3))
  val w = new W[Int, String](1)
  println(w.f(new W[String, Int](""), 3))
  println(w.half(w, 3))
  println(new HK[List](1).f(new HK[Option](2), 3))
  println(new Cov(1).f(new Cov(""), 100000))
  println(new Cov(1).bounded(new Cov("abc"), 100000))
  println(new Contra[Int](1).f(new Contra[String](""), 100000))
  println(v.nested(new V(List("")), 3))
  println(new Cov(1).nested(new Cov(List("a", "b")), 100000))
}
