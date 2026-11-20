// Compiled together with the Java source of `Y`.
object Mixed {
  def ints = Y.arr(1, 2, 3)
  def results: List[Any] = List(
    Y.arr(1, 2, 3).length,
    Y.arr(1, 2, 3)(0) + 1,
    Y.nested(1)(0)(0) + 1,
    new Y.Box(7).items()(1) + 1,
  )
}
