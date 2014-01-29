trait HasA { type A; var p: A = _ }
class AA[T] {
  type HasAT[T] = HasA { type A = T }
  val x: HasAT[T] = ???
  def foo: x.A = ???
}
 
class B extends AA[Int] {
  override def foo: B.this.x.A = super.foo
}