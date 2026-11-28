class Outer {
  def m(p: Int): Int = {
    class C(val v: Int) { def this() = this(p + 1) }
    new C().v + new C(1).v
  }
}

object Test {
  def m(p: Int): Int = {
    class C(val v: Int) { def this() = this(p + 1) }
    new C().v + new C(1).v
  }

  // the constructors differ only in the captured variable
  def n(p: String, q: Int): String = {
    class C(val v: String) {
      def this(a: Int) = this(p + a)
      def this(a: Int, b: Int) = this(a + b + q)
      def this(a: Int, b: String) = this(b + a)
    }
    new C("x").v + " " + new C(1).v + " " + new C(1, 2).v + " " + new C(1, "y").v
  }

  def main(a: Array[String]): Unit = {
    println(m(7))
    println(new Outer().m(7))
    println(n("p", 3))
  }
}
