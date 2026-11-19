import scala.language.reflectiveCalls
import scala.reflect.ClassTag

// `Array[T](elems)(tag)` evaluates the elements, then `tag`, once.
object Test {
  def log[A](s: String, a: A): A = { println(s); a }
  def tag[T](implicit ct: ClassTag[T]): ClassTag[T] = log("tag", ct)
  type Tagged[T] = { def tag: ClassTag[T] }

  def pure[T: ClassTag](x: T, y: T): Array[T]      = Array[T](log("x", x), log("y", y))
  def impure[T: ClassTag](x: T, y: T): Array[T]    = Array[T](log("x", x), log("y", y))(tag[T])
  def pureElems[T: ClassTag](x: T, y: T): Array[T] = Array[T](x, y)(tag[T])
  def nested[T: ClassTag](x: T): Array[T]          =
    Array[T](log("x", x))(log("outer tag", Array[ClassTag[T]](log("inner elem", implicitly[ClassTag[T]]))(tag[ClassTag[T]]).head))
  def structural[T](x: T, t: Tagged[T]): Array[T]  = Array[T](log("x", x))(t.tag)
  def failing[T: ClassTag](x: T): Array[T]         = Array[T](log("x", x))(throw new IllegalStateException("tag"))
  def failingElem[T: ClassTag](x: T): Array[T]     = Array[T](log("x", x), throw new IllegalStateException("elem"))(tag[T])

  def show(name: String)(body: => Array[_]): Unit = {
    println(s"$name:")
    val result = try { val a = body; a.getClass.getSimpleName + a.mkString("(", ", ", ")") } catch { case e: IllegalStateException => e.toString }
    println(s"= $result")
  }

  def main(args: Array[String]): Unit = {
    show("pure")(pure("a", "b"))
    show("impure")(impure("a", "b"))
    show("impure, primitive")(impure(1, 2))
    show("pure elements")(pureElems("a", "b"))
    show("nested")(nested("a"))
    show("structural")(structural("a", new { def tag = log("tag", implicitly[ClassTag[String]]) }))
    show("failing")(failing("a"))
    show("failing element")(failingElem("a"))
  }
}
