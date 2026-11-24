// This test fails: it records that the binder of `case y: 1` on an Int scrutinee is typed Int,
// not 1. With a scrutinee of a primitive type, `==` with the literal implies the value is the
// literal, so the pattern type is a sound type for the binder (on an `Any` scrutinee it is not:
// `1L` also matches). For a String literal the binder already gets the literal type.
object Test {
  def int(a: Int): 1 = a match { case y: 1 => y; case _ => 1 }
  def long(a: Long): 2L = a match { case y: 2L => y; case _ => 2L }
  def str(a: String): "x" = a match { case y: "x" => y; case _ => "x" }
}
