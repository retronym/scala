import scala.annotation.tailrec

// The receiver `o: Tr` has the type of the trait, not its self type `Tr with Product`.
// The call is still on an instance of the same trait, so it can become a jump.
trait Tr { self: Product =>
  @tailrec final def tf(o: Tr, n: Int): Int = if (n == 0) productArity else o.tf(this, n - 1)
  @tailrec final def viaThis(n: Int): Int = if (n == 0) productArity else this.viaThis(n - 1)
}
case class C1(a: Int) extends Tr
case class C2(a: Int, b: Int) extends Tr

class G[A] { self: Serializable =>
  @tailrec final def g(o: G[A], n: Int): Int = if (n == 0) hashCode & 0 else o.g(this, n - 1)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(C1(1).tf(C2(1, 2), 100000))
    println(C1(1).tf(C2(1, 2), 100001))
    println(C2(1, 2).viaThis(100000))
    val g = new G[Int] with Serializable
    println(g.g(new G[Int] with Serializable, 100000))
  }
}
