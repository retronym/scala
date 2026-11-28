object M {
  def f = { trait A; abstract class K extends A; trait C extends K; (null: C) }
  def g = () => f
}
