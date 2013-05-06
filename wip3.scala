import reflect.ClassTag
import scala.collection.{ mutable, immutable, generic }
import mutable.ArrayBuilder

class Foo3 {
  def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T]()(t)

  def concat[T: ClassTag](xss: Array[T]*): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(xss.map(_.length).sum)
    for (xs <- xss) b ++= xs
    b.result()
  }
}

// def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That
// trait CanBuildFrom[-From, -Elem, +To]
