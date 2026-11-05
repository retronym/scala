package ann

object Users {
  def a: Int = Old.old                         // deprecated, from another unit
  def b: Old.Thing = new Old.Thing             // deprecated class
  def c: String = implicitly[Show[Int]].show(1)
  def d: Int = new Holder().m + new UsesJava().m
  @deprecated("users", "2.0") def e: Int = Old.old // no warning inside a deprecated member
  def f = new Holder                           // inferred type, typed in the completer
}
