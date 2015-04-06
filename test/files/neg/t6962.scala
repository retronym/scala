trait Foo {
  def sth() {
    crashes()
  }
  protected[Bar] def crashes(withDefaultParam: Boolean = true) { }
}
