trait X[T <: X[T]]
object Y extends X[Y.type]

trait T {
  // TODO
  // object Z extends X[Z.type]
}
