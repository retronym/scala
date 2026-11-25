// A by-name implicit argument for a type whose search is open is a recursive reference to the
// result of that search. The result is the candidate that static overloading resolution selects,
// whatever the order of the definitions and whichever candidate tied the knot.

trait Show[T] { def show: String }

// Derivation in the style of shapeless: a low priority derived instance with a by-name argument
// for the representation, and a hand-written instance in the companion of the data type.
trait Generic[T] { type Repr }
object Generic {
  implicit val tree: Generic[Tree] { type Repr = List[Tree] } = null
}
trait LowPriorityShow {
  implicit def derived[T, R](implicit gen: Generic[T] { type Repr = R }, repr: => Show[R]): Show[T] =
    new Show[T] { def show = "derived" }
}
object Show extends LowPriorityShow {
  implicit def list[T](implicit elem: => Show[T]): Show[List[T]] = new Show[List[T]] { def show = "list" }
}
class Tree
object Tree {
  implicit val hand: Show[Tree] = new Show[Tree] { def show = "hand" }
}

// The specific instance declared before and after the general one.
trait First[T] { def show: String }
object First {
  implicit val int: First[Int] = new First[Int] { def show = "int" }
  implicit def general[T](implicit rec: => First[T]): First[T] = new First[T] { def show = "general" }
}
trait Last[T] { def show: String }
object Last {
  implicit def general[T](implicit rec: => Last[T]): Last[T] = new Last[T] { def show = "general" }
  implicit val int: Last[Int] = new Last[Int] { def show = "int" }
}

// A candidate that makes a recursive reference and then fails on a later argument.
class Missing
class A(val show: String)
object A {
  implicit def bad(implicit rec: => A, m: Missing): A = new A("bad")
  implicit val ok: A = new A("ok")
}
class B(val show: String)
class B1(show: String) extends B(show)
object B {
  implicit val ok: B1 = new B1("ok")
  implicit def bad(implicit rec: => B, m: Missing): B = new B("bad")
}

object Test {
  def main(args: Array[String]): Unit = {
    println(implicitly[Show[Tree]].show)
    println(implicitly[First[Int]].show)
    println(implicitly[Last[Int]].show)
    println(implicitly[A].show)
    println(implicitly[B].show)
  }
}
