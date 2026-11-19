// `null: _*` passed to a Java varargs method passes a null array,
// as `m((T[]) null)` does in Java.
object Test {
  import scala.tools.testkit.AssertUtil._

  def format = String.format("%%", null: _*)

  def hash = java.util.Objects.hash(null: _*)

  def asList = java.util.Arrays.asList[String](null: _*)

  // the element type is not known to be a reference type
  def generic[T] = java.util.Optional.ofNullable(java.util.Arrays.asList[T](null: _*))

  def nullSeq = String.format("%%", (null: Seq[AnyRef]): _*)

  def nothing: String = String.format("%%", (throw new IllegalStateException("thrown")): _*)

  def main(args: Array[String]): Unit = {
    println(format)
    println(hash)
    assertThrows[NullPointerException](asList)
    assertThrows[NullPointerException](generic[Int])
    assertThrows[NullPointerException](nullSeq)
    assertThrows[IllegalStateException](nothing, _ == "thrown")
  }
}
