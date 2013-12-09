class Casey(a: Int) { def isEmpty = false; def get = this }
object Casey { def unapply(a: Casey) = a }

object Test {
  def main(args: Array[String]) {
    val Casey(x) = new Casey(1)
    assert(x == 1, x)
  }
}
