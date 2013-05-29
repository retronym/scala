class InlinePosition {
  @inline final def foo[A](a: => A) = a

  def client {
    foo {
      "LINE 6".toString
    }
  }
}
