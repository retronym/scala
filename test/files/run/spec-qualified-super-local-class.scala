// The body of `G.k` is duplicated into `G$mcI$sp.k`. The `super[TB]` in the local class `Loc`
// refers to `Loc`'s own parent `TB`, not to the specialized variant that `G$mcI$sp` extends.
trait TB[@specialized(Int) T] { def m(t: T): T = t }
class G[@specialized(Int) T] extends TB[T] {
  def k(t: T) = {
    class Loc extends TB[String] { override def m(x: String) = super[TB].m(x) + "!" }
    new Loc().m(t.toString)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new G[Int].k(1) == "1!")
    assert(new G[String].k("a") == "a!")
  }
}
