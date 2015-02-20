import reflect.runtime.universe._

trait O[A] { trait I }

object Test {
  def main(args: Array[String]): Unit = {
    val t = reify { def f: O[Any]#I = null }
    println(t)
  }
}
