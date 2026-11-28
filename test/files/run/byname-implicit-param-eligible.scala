class Foo(val s: String)
object Foo { implicit val companion: Foo = new Foo("companion") }

class Bar(val s: String)

object Test {
  def foo(implicit f: Foo) = f.s

  // An implicit parameter is eligible in the body of its method (SLS 7.2), ahead of the implicit scope of Foo.
  def byValue(implicit x: Foo) = foo
  def byName(implicit x: => Foo) = foo
  def byNameNoCompanion(implicit x: => Bar) = implicitly[Bar].s

  def main(args: Array[String]): Unit = {
    println(byValue(new Foo("param")))
    println(byName(new Foo("param")))
    println(byNameNoCompanion(new Bar("param")))
  }
}
