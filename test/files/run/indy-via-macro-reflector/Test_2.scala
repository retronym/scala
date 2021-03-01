object Test {
  def main(args: Array[String]) {
    println(new C().foo(null, 0))
  }
}

class C {
  def foo(p1: Object, p2: Int): String = {
    Macro.reflector("dynamic")
  }

}
