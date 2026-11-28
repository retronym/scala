class Test {
  val x = new X
  def m: Unit = x.m()
  def fields: Int = x.f + x.g + x.h + x.i + x.j + x.k + x.l + x.n + x.o
  def p: Unit = x.p(1, 2)
  def q: X.Q = new X.Q
}
