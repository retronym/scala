import annotation.tailrec

// A recursive call on another instance that is turned into a jump must still throw
// a NullPointerException when that instance is null, after evaluating the arguments.
final class C {
  @tailrec def nul(c: C, n: Int): Int = if (n == 0) 1 else c.nul(c, n - 1)
  @tailrec def effects(c: C, n: Int): Int = if (n == 0) 1 else c.effects({ println("c"); c }, { println("n"); n - 1 })
  @tailrec def receiverExpr(cs: List[C], n: Int): Int = if (n == 0) 1 else cs.head.receiverExpr(cs.tail, n - 1)
}

object Test {
  def t(f: => Any): Unit = println(try f catch { case e: NullPointerException => "NPE" })
  def main(args: Array[String]): Unit = {
    val c = new C
    t(c.nul(null, 1))
    t(c.effects(null, 1))
    t(c.receiverExpr(List(c, null), 2))
    // still a loop
    t(c.nul(c, 1000000))
    t(c.receiverExpr(List.fill(100000)(c), 100000))
  }
}
