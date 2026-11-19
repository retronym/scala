package a {
  // same package: implements `J.foo`
  class InA extends J { def foo(): Unit = () }
  trait TA extends J { def foo(): Unit = () }
}

package b {
  // `J.foo` is package-private, so `S.foo` does not implement it (javac: "S is not abstract and does not override abstract method foo() in J")
  class S extends a.J { def foo(): Unit = () }

  // implemented by `InA.foo` and `K.foo`, which are in package `a`
  class S2 extends a.InA { override def foo(): Unit = () }
  class S3 extends a.K { override def foo(): Unit = () }
  class S4 extends a.K

  // a trait in package `a` doesn't help: the JVM selects the abstract `J.foo` before looking at default methods
  class S5 extends a.TA

  abstract class S6 extends a.J { def foo(): Unit = () }
  object O extends S6
}
