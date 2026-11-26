//> using options -Ydelambdafy:inline
import scala.language.existentials

// The result type of `() => f` is an existential over local classes
// whose quantifier bound names another quantifier.
object Test {
  def f = { trait A; abstract class K extends A; class C extends K; new C }
  def g = () => f
  def h = (x: Int) => x + 1
  def k = () => 42

  def main(args: Array[String]): Unit = {
    println(g().getClass.getSuperclass.getName)
    println(g.getClass.getGenericSuperclass)
    println(h(1))
    println(h.getClass.getGenericSuperclass)
    println(k.getClass.getGenericSuperclass)
  }
}
