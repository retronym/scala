object Test {
  trait Foo[CC[+X], +A]
  // _ is kind polymorphic
  def f[A](x: Foo[_, A]): A = ???
}
