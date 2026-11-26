import scala.language.reflectiveCalls
class K { def m(x: Int): Unit = (); def n: Unit = () }
trait HasN { def n: Any }
class KN extends HasN { def n: Unit = () }
object S {
  def show(x: Any): String = if (x == null) "null" else x.toString
  def main(args: Array[String]): Unit = {
    println("structural m(1): " + show((new K: { def m(x: Int): Any }).m(1)))
    println("structural n:    " + show((new K: { def n: Any }).n))
    println("through a trait: " + show((new KN: HasN).n))
  }
}
