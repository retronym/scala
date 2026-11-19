// A super call in a specialized class that extends a specialized trait. The body of `G.m` is duplicated
// into `G$mcI$sp.m$mcI$sp`; its super call must reach the trait's implementation, not dispatch back.

trait TB[@specialized(Int) T] { def m(t: T): T = t }
class G[@specialized(Int) T] extends TB[T] { override def m(t: T) = super.m(t) }
class GO[@specialized(Int) T] extends TB[T] {
  override def m(t: T) = t
  def viaSuper(t: T) = super.m(t)
}

object Calls { var log = List.empty[String] }
trait TC[@specialized(Int) T] extends TB[T] { override def m(t: T): T = { Calls.log ::= "TC.m"; super.m(t) } }
class GC[@specialized(Int) T] extends TC[T] { override def m(t: T) = { Calls.log ::= "GC.m"; super.m(t) } }

object Test {
  def t(label: String)(f: => Any): Unit =
    try println(s"$label: $f ${Calls.log.reverse.mkString(" ")}")
    catch { case _: StackOverflowError => println(s"$label: StackOverflowError") }
    finally Calls.log = Nil

  def main(args: Array[String]): Unit = {
    t("Int")(new G[Int].m(1))
    t("Int, via trait")((new G[Int]: TB[Int]).m(2))
    t("String")(new G[String].m("s"))
    t("other method")(new GO[Int].viaSuper(4))
    t("two traits")(new GC[Int].m(5))
    t("two traits, String")(new GC[String].m("c"))
  }
}
