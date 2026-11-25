import scala.reflect.ClassTag

class Foo
object Foo {
  implicit val ct: ClassTag[Foo] = new ClassTag[Foo] { def runtimeClass = classOf[Foo]; override def toString = "Foo.ct" }
}

class Baz
object Baz {
  implicit val m: Manifest[Baz] = new Manifest[Baz] { def runtimeClass = classOf[Baz]; override def toString = "Baz.m" }
}

class One
object One { implicit val v: ValueOf[One] = new ValueOf(new One { override def toString = "One.v" }) }

class TC[A](val s: String)
class Bar
object Bar { implicit val tc: TC[Bar] = new TC("Bar.tc") }

object Test {
  def tag[T](implicit ct: ClassTag[T]) = ct
  def main(args: Array[String]): Unit = {
    println(implicitly[TC[Bar]].s)       // an ordinary type class in the companion is found
    println(implicitly[Manifest[Baz]])   // SLS 7.5: an implicit argument that matches is selected, otherwise a manifest is synthesized
    println(implicitly[ClassTag[Foo]])
    println(tag[Foo])
    println(implicitly[ValueOf[One]].value)
    println(implicitly[ClassTag[Bar]])   // nothing in the implicit scope: synthesized
  }
}
