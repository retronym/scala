// Compiled against the class file of `Y`.
class Holder { def ints = Y.arr(1, 2, 3) }

object Test {
  def main(args: Array[String]): Unit = {
    println(Y.arr("a", "b").length)
    println(Y.arr[Integer](1, 2).length)
    println(Y.arr(1, 2, 3).length)
    println(Y.arr[Int](1, 2, 3).length)
    println(Y.arr(1, 2, 3)(0) + 1)
    println(Y.arr(1, 2, 3).map(_ * 2).toList)
    println(Y.arr((), ()).length)
    println(Y.nested(1)(0)(0) + 1)
    println(new Y.Box(7).items().toList)
    println(new Y.Box(7).items()(1) + 1)
    println(new Holder().ints.length)
    println(classOf[Holder].getMethod("ints").toGenericString)
    println(Mixed.ints.length)
    println(Mixed.results)
  }
}
