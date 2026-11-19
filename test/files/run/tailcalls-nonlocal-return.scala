import annotation.tailrec

// The try/catch that UnCurry wraps around the body of a method with a non-local return
// does not prevent tail call elimination.
object Test {
  @tailrec def find(n: Int, xs: List[Int]): Int = {
    xs.foreach(x => if (x == n) return x)
    if (n > 1000000) -1 else find(n + 1, xs)
  }

  // A non-local return from a closure created in an earlier iteration returns from the
  // outermost call, as it would from the frame that created the closure.
  @tailrec def early(n: Int, fs: List[() => Unit]): Int = {
    val f: () => Unit = () => return n
    if (n == 3) { fs.last(); -1 }
    else early(n + 1, f :: fs)
  }

  // A closure that escapes the method still throws NonLocalReturnControl when called later.
  var escaped: () => Unit = null
  @tailrec def escape(n: Int): Int = {
    if (n == 0) escaped = () => return 42
    if (n < 3) escape(n + 1) else n
  }

  final class C(val id: Int) {
    @tailrec def sibling(o: C, n: Int): Int = {
      List(1).foreach(_ => if (n == 0) return id)
      o.sibling(this, n - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(find(0, List(-5)))
    println(find(0, List(1000)))
    println(early(0, Nil))
    println(escape(0))
    println(try { escaped(); "no exception" } catch { case e: scala.runtime.NonLocalReturnControl[_] => e.value })
    println(new C(1).sibling(new C(2), 1000001))
  }
}
