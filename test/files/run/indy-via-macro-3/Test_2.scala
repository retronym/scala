import Macro._

object Test {
  def main(args: Array[String]) {
    val f = new Foo()
    compareAndSetInt(classOf[Foo], "blah", f, -1, 0)
    assert(!compareAndSetInt(classOf[Foo], "blah", f, -1, 0))
    assert(compareAndSetInt(classOf[Foo], "blah", f, 42, 0))
    assert(f.blah == 0)
  }
}

class Foo {
  var blah = 42
}
