//> using options -nowarn

// `==` between a number or character and a value whose static type is a trait or interface has to
// compare cooperatively: the value can be a ScalaNumber such as BigInt, whose `equals` knows about
// the boxes but which the boxes' `equals` do not know about.
object Test {
  trait T
  class N(n: Int) extends scala.math.ScalaNumber with T {
    def underlying = Int.box(n)
    def isWhole = true
    def intValue = n; def longValue = n.toLong; def floatValue = n.toFloat; def doubleValue = n.toDouble
    override def equals(o: Any) = o match {
      case i: Int => i == n
      case _ => false
    }
    override def hashCode = n
  }

  def main(args: Array[String]): Unit = {
    val o: Ordered[BigInt] = BigInt(1)
    assert(1 == o)
    assert(o == 1)
    assert(1 == (o: Any))
    assert(!(1 != o))
    assert(1L == o)
    assert(1.0 == o)
    assert((1: Integer) == o)
    assert(2 != o)

    val c: Ordered[BigInt] = BigInt(97)
    assert('a' == c)
    assert(c == 'a')

    val t: T = new N(3)
    assert(3 == t)
    assert(t == 3)
    assert(4 != t)

    val s: Iterable[Int] = List(1)
    assert(1 != s)
  }
}
