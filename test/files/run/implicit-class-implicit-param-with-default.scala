object Test {
  implicit class C(self: String)(implicit val foo: String = "default") {
    def test = foo
  }

  implicit class WorkaroundOk(self: String)(implicit val foo: String) {
    def this(self: String, dummy: AnyRef = null) { this(self)("")}
  }

  def main(args: Array[String]) {
    println("".foo)
    println(C("").foo)
    println(new C("").foo)
    println(C("")("explicit").foo)
    println(new C("")("explicit").foo)
  }
}
