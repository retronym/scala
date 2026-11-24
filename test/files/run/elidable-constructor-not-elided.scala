// At the default -Xelide-below level, a constructor annotated with @elidable is not elided, so it is accepted.
import annotation._, elidable._

class E @elidable(INFO) () { val z = 5 }
class F(val y: Int) {
  @elidable(INFO) def this() = this(1)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new E().z)
    println(new F().y)
  }
}
