class Foo(val s: String)

object Outer {
  implicit val x: Foo = new Foo("Outer.x")

  class C(val s: String) {
    implicit val x: Foo = new Foo("C.x")
    // The members of C are not in scope in a self constructor call (SLS 5.3.1): `x` is Outer.x.
    def this(i: Int) = this(x.s)
    def this(b: Boolean) = this(implicitly[Foo].s)
    def this(c: Char, t: String = implicitly[Foo].s) = this(t)
    def this(d: Double) = { this(implicitly[Foo].s); assert(implicitly[Foo].s == "C.x") }
    def m = implicitly[Foo].s
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new Outer.C(1).s)
    println(new Outer.C(true).s)
    println(new Outer.C('c').s)
    println(new Outer.C(1.0).s)
    println(new Outer.C("").m)
  }
}
