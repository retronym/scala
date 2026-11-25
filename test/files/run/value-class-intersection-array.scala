//> using options -nowarn
trait Tagged extends Any
class Meters(val n: Int) extends AnyVal with Tagged { override def toString = s"Meters($n)" }

object Test {
  def show(label: String)(body: => Any): Unit =
    println(label + " = " + (try String.valueOf(body) catch { case t: Throwable => t.getClass.getName }))

  def bounded[T <: Meters](t: T): List[Any] = List(t)
  def intersect[T](t: T with Meters): List[Any] = List(t)
  def direct(v: Meters): List[Any] = List(v)
  def compound(v: Meters with Tagged): List[Any] = List(v)
  def arr(v: Meters with Tagged) = Array(v)
  def upcast(a: Array[Meters with Tagged]): Array[Meters] = a

  def main(args: Array[String]): Unit = {
    val m = new Meters(1)
    show("direct(m)")(direct(m))
    show("direct(m).head.isInstanceOf[Meters]")(direct(m).head.isInstanceOf[Meters])
    show("compound(m)")(compound(m))
    show("compound(m).head.isInstanceOf[Meters]")(compound(m).head.isInstanceOf[Meters])
    show("bounded(m)")(bounded(m))
    show("intersect[Any](m)")(intersect[Any](m))
    show("arr(m).getClass")(arr(m).getClass.getSimpleName)
    show("Array(m).getClass")(Array(m).getClass.getSimpleName)
    show("upcast(arr(m))(0)")(upcast(arr(m))(0))
    show("classTag[Meters with Tagged]")(scala.reflect.classTag[Meters with Tagged])
  }
}
