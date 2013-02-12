object Test {
  def unapply[T](a: Any)(implicit T: Manifest[T]) = Some(0)

  //val Test[Int](a) = 1
  1 match {
    case Test[Int](a) =>
  }
}