trait B {
  type T <: java.io.Serializable
  def f(x: T) = x.eq(x)

  type U
  def g(x: U) = x.eq(x)

  type V <: Any
  def h(x: V) = x.eq(x)

  type W <: java.io.Serializable with Comparable[W]
  def i(x: W) = x.eq(x)

  def j[X <: Comparable[X]](x: X) = x.eq(x)

  def k[Y <: AnyVal](y: Y) = y.eq(y)
}
