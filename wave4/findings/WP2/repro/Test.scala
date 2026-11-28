import scala.language.reflectiveCalls
class Box[A](val v: A) { def get(n: Int): A = v }
case class V0[T](u: T) extends AnyVal {
  def m: List[T] = { val s: { def get(n: Int): List[T] } = new Box(List(u)); s.get(1) }
}
object Test { def main(a: Array[String]): Unit = println(V0("x").m) }
