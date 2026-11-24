@A(1) class Test {
  def a: Class[A] = classOf[A]
  def b: Class[B] = classOf[B]
  def c: Class[C] = classOf[C]
  def f: Int = new UsesA().f
}
