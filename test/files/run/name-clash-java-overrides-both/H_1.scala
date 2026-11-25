// `m(x: Any)` and `m(x: AnyRef)` do not match and have the same erasure. A Java method `m(Object)` overrides both.
trait H1 { def m(x: Any): String }
trait H2 { def m(x: AnyRef): String }

// JC is seen as a Java source here and as a class file in Test_2.scala
class K1 extends JC
