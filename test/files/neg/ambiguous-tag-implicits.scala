import scala.reflect.ClassTag

class Foo

object Control {
  implicit val a: Ordering[Foo] = null
  implicit val b: Ordering[Foo] = null
  def noExpectedType                = implicitly[Ordering[Foo]]
  def expectedType: Ordering[Foo]   = implicitly[Ordering[Foo]]
}

// Two eligible implicits, neither more specific: an error, with or without an expected type (SLS 7.2, 7.5).
object Manifests {
  implicit val a: Manifest[Foo] = null
  implicit val b: Manifest[Foo] = null
  def noExpectedType                = implicitly[Manifest[Foo]]
  def expectedType: Manifest[Foo]   = implicitly[Manifest[Foo]]
}

object ClassTags {
  implicit val a: ClassTag[Foo] = null
  implicit val b: ClassTag[Foo] = null
  def noExpectedType                = implicitly[ClassTag[Foo]]
  def expectedType: ClassTag[Foo]   = implicitly[ClassTag[Foo]]
}

object ValueOfs {
  implicit val a: ValueOf[1] = new ValueOf[1](1)
  implicit val b: ValueOf[1] = new ValueOf[1](1)
  def noExpectedType                = implicitly[ValueOf[1]]
  def expectedType: ValueOf[1]      = implicitly[ValueOf[1]]
}
