package annneg

object UsesMissing2 {
  def b = implicitly[Show[Int]] + UsesMissing.old
}
