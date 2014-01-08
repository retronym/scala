// implicits on get
class Casey2(val a: Int) { def isEmpty: Boolean = false; def get(implicit X: DummyImplicit) = a }
object Casey2 { def unapply(a: Casey2) = a }

object Test {
  def main(args: Array[String]) {
    val Casey2(a3) = new Casey2(1)
  }
}
