//> using options -Xdev
// Unboxing `null` to a primitive type gives its zero; -Xdev must not reject the unboxing.
class C[@specialized(Byte, Int) T](val key: AnyRef, val value: T)

object Test {
  def f: Int = null.asInstanceOf[Int]
  def g: Byte = (null: Null).asInstanceOf[Byte]
  def main(args: Array[String]): Unit = {
    assert(f == 0 && g == 0)
    assert(new C[Byte]("k", 1.toByte).value == 1)
  }
}
