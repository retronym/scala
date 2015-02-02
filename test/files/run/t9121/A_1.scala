package p1

object Implicits {
  class ScalaObservable(val underlying: Any) extends AnyVal {
    //if remove `@inline` annotation then test will success
    @inline def scMap[R](f: String): Any = f.toRx
  }

  implicit class RichFunction1[T1, R](val f: String) extends AnyVal {
    def toRx: Any = ""
  }
}
