trait C {
  def foo(i: Int): Unit
  def bar(implicit i: Int): Unit
}

trait D extends C {
  def foo(implicit i: Int): Unit
  def bar(i: Int): Unit
}

