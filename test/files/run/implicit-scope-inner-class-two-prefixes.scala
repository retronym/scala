class Pair[A, B](val s: String)
class Outer {
  class Inner
  object Inner {
    implicit def l[X]: Pair[Inner, X] = new Pair("l")
    implicit def r[X]: Pair[X, Inner] = new Pair("r")
  }
}
object O { val outer = new Outer }
// a static class with the base type O.outer.Inner
class S extends O.outer.Inner
// a static class that mentions O.outer.Inner in a type argument of a base type
class T extends Ordered[O.outer.Inner] { def compare(that: O.outer.Inner) = 0 }

object Test {
  val a = new Outer
  def main(args: Array[String]): Unit = {
    // The parts of the two types are the same, so are their implicit scopes: the companions a.Inner and O.outer.Inner.
    println(implicitly[Pair[S, a.Inner]].s)
    println(implicitly[Pair[a.Inner, S]].s)
    println(implicitly[Pair[T, a.Inner]].s)
    println(implicitly[Pair[a.Inner, T]].s)
  }
}
