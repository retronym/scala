//> using options -Xsource:3 -Xsource-features:implicit-resolution

// A subclass of a contravariant class is more specific than its parent
// at any nesting depth, not just at the top level.

trait Show[-T] { def n: String }
trait SubShow[-T] extends Show[T]
trait Box[+A] { def n: String }

object Top {
  implicit val i1: Show[String]    = new Show[String]    { def n = "i1" }
  implicit val i2: SubShow[String] = new SubShow[String] { def n = "i2" }
}

object Nested {
  implicit val i1: Box[Show[String]]    = new Box[Show[String]]    { def n = "i1" }
  implicit val i2: Box[SubShow[String]] = new Box[SubShow[String]] { def n = "i2" }
}

object NestedWider {
  implicit val i1: Box[Show[CharSequence]] = new Box[Show[CharSequence]] { def n = "i1" }
  implicit val i2: Box[SubShow[String]]    = new Box[SubShow[String]]    { def n = "i2" }
}

object Deep {
  implicit val i1: Box[Box[Show[String]]]    = new Box[Box[Show[String]]]    { def n = "i1" }
  implicit val i2: Box[Box[SubShow[String]]] = new Box[Box[SubShow[String]]] { def n = "i2" }
}

object Test {
  def main(args: Array[String]): Unit = {
    println({ import Top._; implicitly[Show[String]].n })
    println({ import Nested._; implicitly[Box[Show[String]]].n })
    println({ import NestedWider._; implicitly[Box[Show[String]]].n })
    println({ import Deep._; implicitly[Box[Box[Show[String]]]].n })
  }
}
