// Default getters in the same parameter list as inferred arguments receive
// the type arguments solved for the application, not `Nothing`.
import scala.collection.mutable.ListBuffer

class Box[T](val a: T, val b: T = null.asInstanceOf[T]) { def get: T = if (b == null) a else b }
class Box3[T](a: T, b: T = null.asInstanceOf[T])(c: T = a) { override def toString = "" + a + b + c }

object Test {
  def pick[T](a: T, b: T = null.asInstanceOf[T]): T = if (b == null) a else b
  def pickL[T](a: T)(b: T = null.asInstanceOf[T]): T = if (b == null) a else b
  def pair[T](a: T, b: T = null.asInstanceOf[T]): (T, T) = (a, b)
  def three[T](a: T, b: T = null.asInstanceOf[T])(c: T = a): String = "" + a + b + c
  def add[T](a: T, buf: ListBuffer[T] = new ListBuffer[T]): ListBuffer[T] = buf += a
  // the default contributes to inference
  def fromDefault[T](x: T = 1): T = x
  def opt[T](a: T, b: Option[T] = None): String = "" + a + b

  def main(args: Array[String]): Unit = {
    println(pick("a"))
    println(pick[String]("a"))
    println(pick(a = "a"))
    println(pickL("a")())
    println(pair("a"))
    println(three("s")())
    println(three(1)())
    println(add(1))
    println(fromDefault())
    println(opt(1))
    println(new Box("a").get)
    println(new Box(a = "a").get)
    println(new Box[String]("a").get)
    println(new Box3("s")())
    println(new Box3(a = "s")())
    println(new Box3(1)(c = 2))
  }
}
