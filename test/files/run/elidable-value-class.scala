//> using options -Xelide-below WARNING
//
import annotation._, elidable._

class V(val x: Int) extends AnyVal
class S(val s: String) extends AnyVal
class G[T](val t: T) extends AnyVal
class P private (val x: Long) extends AnyVal
object P { def apply(x: Long): P = new P(x) }

object Test {
  type Alias = V

  @elidable(INFO) def v(): V = new V(3)
  @elidable(INFO) def s(): S = new S("s")
  @elidable(INFO) def gi(): G[Int] = new G(1)
  @elidable(INFO) def gs(): G[String] = new G("g")
  @elidable(INFO) def p(): P = P(1L)
  @elidable(INFO) def alias(): Alias = new V(3)

  def main(args: Array[String]): Unit = {
    // the elided result is a value of the value class, so its members can be selected
    println(v().x)
    println(s().s.length)
    println(gi().t)
    println(gs().t)
    println(p().x)
    println(alias().x)
    // boxed
    println((v(): Any) == new V(0))
    println(List(v()).head.x)
    println((p(): Any) == P(0L))
    // the method definition is still available and returns the same value
    println(Test.getClass.getMethod("v").invoke(Test))
  }
}
