import scala.language.{existentials, higherKinds}
class Functor[F[_]]
object Holder {
  val someG: Functor[G] forSome { type G[x] <: List[x] } = new Functor[::]
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = Class.forName("Holder$")
    println(try c.getDeclaredField("someG").getGenericType.toString catch { case t: Throwable => t.toString })
    println(try c.getDeclaredMethod("someG").getGenericReturnType.toString catch { case t: Throwable => t.toString })
  }
}
