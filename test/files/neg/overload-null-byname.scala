object Test {
  def g(x: => Int): Int = x
  def m(x: => Int): Int = x; def m(x: => Long)(implicit d: DummyImplicit): Long = x

  def t1 = g(null)
  def t2 = m(null)
}
