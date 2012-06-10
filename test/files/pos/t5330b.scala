trait Base[T] {
  def foo: this.type
  def bar: Base[T]
}

class Derived[T] extends Base[T] {
  def foo = sys.error("!!!")
  def bar = sys.error("!!!")
}
