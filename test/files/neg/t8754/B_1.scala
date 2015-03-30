trait Base[T] {
  def peer: Component
}

trait B[T] extends Base[T] {
  def peer: Component {}
}
