object Test {
  def first[CC[X] <: Iterable[X], A](c: CC[A]): A = c.head

  first(List(1))
  first[Iterable, (String, Int)](Map("a" -> 1))
  // CC = Iterable, A = (String, Int) is the only solution within the bound
  val kv: (String, Int) = first(Map("a" -> 1))
}
