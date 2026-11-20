import scala.language.reflectiveCalls

// A type parameter bounded by a value class erases to the underlying type, like the value class itself
// (scala/bug#6336): a reflective call would pass the boxed value to a method that takes an `int`.
class V(val x: Int) extends AnyVal
class K { def p[A <: V](a: A) = a.x; def r[A <: V](a: A): A = a }

object Test {
  val k: { def p[A <: V](a: A): Int } = new K
  val l: { def r[A <: V](a: A): A } = new K
  def q[B <: V]: { def p[A <: B](a: A): Int } = new K // only the error for an abstract type defined outside
  def main(args: Array[String]): Unit = println(k.p(new V(1)))
}
