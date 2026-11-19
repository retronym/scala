package a {
  // same package: overrides `J.bar`
  class InA extends J { override def bar(): String = "InA.bar" }
}

package b {
  // `J.bar` is package-private, so `S.bar` does not override it, as in Java
  class S extends a.J { def bar(): String = "S.bar" }
  class S2 extends a.InA { override def bar(): String = "S2.bar" }
  // `S3.foo` overrides the abstract, package-private `JA.foo` through the public `KA.foo`
  class S3 extends a.KA { def foo(): String = "S3.foo" }
}

object Test {
  def main(args: Array[String]): Unit = {
    val s = new b.S
    println(s.bar())
    println(s.callBar())
    val s2 = new b.S2
    println(s2.bar())
    println(s2.callBar())
    println(new b.S3().callFoo())
  }
}
