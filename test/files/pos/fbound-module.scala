trait X[T <: X[T]]
object Y extends X[Y.type]

trait T {
  object Z extends X[Z.type]

  def local = {
    object Z extends X[Z.type]
  }
}
