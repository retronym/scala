//> using options -Wconf:cat=deprecation:s
// a @tailrec method nested in a local method of a self-constructor argument
class P(val res: Int) {
  def this(p: Int, q: Int) = this({
    def loc(): Int = {
      @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + q)
      loop(p, 0)
    }
    loc()
  })
}
// in a super-constructor argument, nested in a local value
class Q(p: Int, q: Int) extends P({
  val x = {
    @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + q)
    loop(p, 1)
  }
  x
})
// in an early initializer
class R(p: Int, q: Int) extends {
  val x = {
    def loc(): Int = {
      @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + q)
      loop(p, 2)
    }
    loc()
  }
} with P(x)
// not tail recursive
class S(val res: Int) {
  def this(p: Int, q: Int) = this({
    def loc(): Int = {
      def loop(i: Int, acc: Int): Int = if (i == 0) acc else 1 + loop(i - 1, acc + q)
      loop(p, 0)
    }
    loc()
  })
}
// directly in the argument
class T(val res: Int) {
  def this(p: Int, q: Int) = this({
    @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + q)
    loop(p, 0)
  })
}
// after the self-constructor call, where `this` is available
class U(var res: Int) {
  def get = res
  def this(p: Int, q: Int) = {
    this(0)
    def loc(): Int = {
      @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc + get else loop(i - 1, acc + q)
      loop(p, 0)
    }
    res = loc()
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    assert(new P(3, 10).res == 30)
    assert(new Q(3, 10).res == 31)
    assert(new R(3, 10).res == 32)
    assert(new S(3, 10).res == 33)
    assert(new T(3, 10).res == 30)
    assert(new U(3, 10).res == 30)
  }
}
