// The private member is protected after specialization; it stays accessible on another
// instance from the body duplicated into the specialized subclass.
@SerialVersionUID(1L)
final class SparseArray[@specialized(Int) T](private var data: Array[T]) extends Serializable {
  def use(inData: Array[T]) = {
    data = inData;
  }

  def set(that: SparseArray[T]) = {
    use(that.data.clone)
  }

  def get = data
}

object Test {
  def generic[T](a: Array[T]) = new SparseArray[T](a)

  def main(args: Array[String]): Unit = {
    val a = new SparseArray(Array(1, 2))
    a.set(new SparseArray(Array(3, 4)))
    println(a.get.toList)
    a.set(generic(Array(5, 6)))
    println(a.get.toList)
    val g = generic(Array(7))
    g.set(a)
    println(g.get.toList)
  }
}
