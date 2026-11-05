package annneg

object UsesMissing {
  def a = implicitly[Show[String]]
  @deprecated("x", "1") def old = 1
}
