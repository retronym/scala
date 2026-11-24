// A method whose body is `synchronized { ... }` is emitted with ACC_SYNCHRONIZED, which locks
// the instance it is entered on. A recursive call on another instance must lock that one, so it
// is not turned into a jump.
final class C(val name: String) {
  def other(o: C, n: Int): Boolean = synchronized { if (n == 0) Thread.holdsLock(this) else o.other(o, n - 1) }
  def same(n: Int): Boolean = synchronized { if (n == 0) Thread.holdsLock(this) else same(n - 1) }
  def sameExplicit(n: Int): Boolean = synchronized { if (n == 0) Thread.holdsLock(this) else this.sameExplicit(n - 1) }
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new C("a")
    println(a.other(new C("b"), 1))
    // still a loop when the receiver is `this`
    println(a.same(1000000))
    println(a.sameExplicit(1000000))
  }
}
