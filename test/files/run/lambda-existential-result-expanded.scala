//> using options -Wconf:cat=deprecation:s
import scala.language.existentials

// A lambda in a super constructor argument or an early definition is expanded
// to an anonymous class, also under the default -Ydelambdafy:method.
object E {
  def f = { trait A; abstract class K extends A; class C extends K; new C }
}
class P(val g: () => Any)
class Q extends P(() => E.f)
class R extends { val g = () => E.f } with AnyRef

object Test {
  def main(args: Array[String]): Unit = {
    println(new Q().g().getClass.getSuperclass.getName)
    println(new R().g().getClass.getSuperclass.getName)
  }
}
