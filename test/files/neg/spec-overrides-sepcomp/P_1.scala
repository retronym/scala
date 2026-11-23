class P {
  def a[@specialized(Int) T](t: T): List[T] = List(t)
}
