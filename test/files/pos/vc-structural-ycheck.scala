//> using options -Ycheck:extmethods,pickler,refchecks,uncurry,fields,tailcalls

import scala.language.reflectiveCalls

// A structural call in a method of a generic value class, whose refinement mentions the class's type parameter.
// The check stops before erasure, where -Ycheck has unrelated trouble with generic value classes (retronym/scala#427).
class Box[A](val v: A) { def get(n: Int): A = v }
class V0[T](val u: T) extends AnyVal {
  def m: List[T] = { val s: { def get(n: Int): List[T] } = new Box(List(u)); s.get(1) }
}
