import scala.reflect.ClassTag

trait A { def a = "a" }
trait B { def b = "b" }
class AB extends A with B

object Test {
  def t(name: String)(body: => Any): Unit =
    try println(name + ": " + body)
    catch { case e: ClassCastException => println(name + ": ClassCastException") }

  def len(xs: Array[A with B]): Int = xs.length
  def mk[T <: A with B: ClassTag](x: T): Array[T] = Array(x)

  def main(args: Array[String]): Unit = {
    // no casts anywhere: scalac treats Array[A with B] and Array[B with A] as the same type,
    // but erases them to A[] and B[]
    t("assign")  { val xs: Array[A with B] = Array[B with A](new AB); xs.length }
    t("argument"){ len(Array[B with A](new AB)) }
    t("element") { val l: List[Array[A with B]] = List(Array[B with A](new AB)); l.head.length }
    t("classtag"){ mk[B with A](new AB).length }
  }
}
