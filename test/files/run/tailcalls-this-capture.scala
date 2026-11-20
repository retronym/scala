import annotation.tailrec
import java.io._

// A tail-recursive local method that does not refer to `this` is lifted to a static method,
// so a lambda that calls it does not capture the enclosing instance.
class NotSer {
  def withTailCall: Int => Int = { x =>
    @tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + x)
    loop(x, 0)
  }
  def withoutTailCall: Int => Int = { x =>
    def loop(i: Int, acc: Int): Int = if (i == 0) acc else acc + x
    loop(x, 0)
  }
}

object Test {
  def roundTrip(f: Int => Int): Int => Int = {
    val bos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(f)
    oos.close()
    new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray)).readObject().asInstanceOf[Int => Int]
  }
  def main(args: Array[String]): Unit = {
    println(roundTrip(new NotSer().withoutTailCall)(3))
    println(roundTrip(new NotSer().withTailCall)(3))
    println(new NotSer().withTailCall(100000))
  }
}
