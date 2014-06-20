trait T {
  var called0 = false
  var called1 = false
  val f1 = {called1 = true; ()}
  var called2 = false
  val f2 = {called2 = true; ()}
  
  identity(f1)
  def test = identity(f2)
}

class C extends T

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    assert(c.called1)
    assert(c.called2)
    c.test
  }
}
