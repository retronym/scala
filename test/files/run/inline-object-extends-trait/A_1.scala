package pack

trait T {
  self: O.type =>
  def bar = ""
}

object O {
  @inline def foo(a: Any) = a
}
