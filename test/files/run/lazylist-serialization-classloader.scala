import java.io._

// Partest loads the test classes in a child of the library's class loader. Deserializing an unevaluated
// LazyList resolves the user classes captured by its state; that must not happen under a frame of a
// library `readObject`, or `ObjectInputStream.resolveClass` uses the library's loader and cannot see them.
case class Box(n: Int)
case class Holder(xs: LazyList[Any])

object Test {
  def roundTrip[A](a: A): A = {
    val bos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(a)
    oos.close()
    new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray)).readObject().asInstanceOf[A]
  }

  def check(name: String)(mk: => Any)(show: Any => Any): Unit = {
    val r = try show(roundTrip(mk)) catch { case e: Exception => e }
    println(s"$name: $r")
  }

  def main(args: Array[String]): Unit = {
    val b = Box(1)
    def iterated = LazyList.iterate(1)(x => x + 1)
    def continual = LazyList.continually(b)
    def mapped = List(1, 2, 3).to(LazyList).map(x => Box(x))
    val take3 = (x: Any) => x.asInstanceOf[LazyList[Any]].take(3).toList
    val holderTake3 = (x: Any) => take3(x.asInstanceOf[Holder].xs)
    check("iterate")(iterated)(take3)
    check("continually")(continual)(take3)
    check("map")(mapped)(take3)
    check("Holder(iterate)")(Holder(iterated))(holderTake3)
    check("Holder(continually)")(Holder(continual))(holderTake3)
  }
}
