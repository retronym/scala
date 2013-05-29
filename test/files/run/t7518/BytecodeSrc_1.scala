class InlinePosition {
  @inline final def foo[A](a: () => A) = a()

  def client {
    foo {
      () => "LINE 6".toString // inlined closure body can/should be at line 6
    }
    foo {
      OtherFile.fun // inlined instructions should be at line 8, the call to foo.
    }
  }
}
