object one {
  def foo() = { object Foo { class Bar } ; new Foo.Bar }
  def module() = { trait T { object Bar } ; (new T {}).Bar }
}
