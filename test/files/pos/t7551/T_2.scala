package p

@A_1(subInterface = classOf[T.S])
trait T {
}

object T {
  private[p] trait S extends T { }
}
