// The type pattern `_: o.type` tests `o eq x`; the stable identifier pattern `p` tests `p == x`.
// When `p`'s type is `o.type`, the two tests are still different for a class with its own `equals`.
// This test fails on 2.13.18 and on this branch: the second case of `alias` and `string` is unreachable.
class C { override def equals(o: Any) = o.isInstanceOf[C]; override def hashCode = 0 }

sealed trait T
case object M extends T
case object N extends T

object Test {
  val o = new C
  val p2: o.type = o
  val s = "s"
  val t: s.type = s

  def alias(a: Any) = a match { case _: o.type => "eq"; case `p2` => "equals"; case _ => "n" }
  def same(a: Any) = a match { case _: o.type => "eq"; case `o` => "equals"; case _ => "n" }
  def string(a: Any) = a match { case _: s.type => "eq"; case `t` => "equals"; case _ => "n" }

  // the type pattern and the stable identifier pattern of a case object still line up for exhaustivity
  def exhaustive1(x: T) = x match { case _: M.type => "M"; case N => "N" }
  def exhaustive2(x: T) = x match { case M => "M"; case _: N.type => "N" }

  def main(args: Array[String]): Unit = {
    println(List(alias(o), alias(new C), alias(1)).mkString(", "))
    println(List(same(o), same(new C), same(1)).mkString(", "))
    println(List(string(s), string(new String("s")), string(1)).mkString(", "))
    println(List(exhaustive1(M), exhaustive1(N), exhaustive2(M), exhaustive2(N)).mkString(", "))
  }
}
