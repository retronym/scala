import reflect._

object Test {
  class Foo(val n: Int) extends AnyVal
  type F = Foo

  def main(args: Array[String]) {
    val ct1 = implicitly[ClassTag[Foo]]
    val ct2 = implicitly[ClassTag[F]]
    println(ct1)
    println(ct2)
  }

}
