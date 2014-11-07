trait Foo {
  trait Bar {
    private[Foo] def baz
  }

  object Blip extends Bar {
    def baz { }
  }
}

object Test extends App {
  val foo = (new Foo {})
  foo.Blip.baz
}
