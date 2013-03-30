trait A[@specialized(Byte) T1] {
  def f[@specialized(Float) T2](x: T1, y: T2): Any = 5
}

trait B extends A[Byte] {
  override def f[@specialized(Float) T2](x: Byte, y: T2): String = ""
}
