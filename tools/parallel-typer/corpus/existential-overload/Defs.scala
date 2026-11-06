package exov

import scala.reflect.ClassTag

// Found by pekko's javadsl: `c.apply`'s member type skolemizes the wildcard of `c` when it is computed, and overload
// resolution types the argument twice. It compiles only because `MethodSymbol.typeAsMemberOf` caches the result.
trait P[-T] { def apply(t: T): Unit }

class D {
  def on[T <: Throwable](f: Throwable => Unit)(implicit tag: ClassTag[T]): D = this
  def on(p: Throwable => Boolean)(f: Throwable => Unit): D = this
}
