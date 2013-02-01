trait A {
  def apply(xs: Seq[Int]): Any = ???
}

trait C {
  def apply(xs: Int*): String = ???
}

object O extends A with C

object Test {
  def main(args: Array[String]) {
    //val a: A = new B
    //a(1,2,3) // prints "called B"
  }
}