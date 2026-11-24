//> using options -Ycheck:fields

// The tree checker re-types templates after fields. Its double-definition check
// took `object f` and the accessor `f` that fields adds to implement `T.f` for
// a double definition and unlinked the accessor, so erasure emitted no bridge.

trait T { def f: Object }
object O extends T { object f }

abstract class A { def g: Object }
object P extends A { object g }

object Test {
  def main(args: Array[String]): Unit = {
    println((O: T).f eq O.f)
    println((P: A).g eq P.g)
  }
}
