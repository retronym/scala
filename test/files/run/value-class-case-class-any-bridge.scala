// A value case class whose underlying type erases to Object: the synthetic companion extends
// `AbstractFunction1[Any, CA]`, and its `apply(a: Any): CA` erases to `apply(Object): Object`, the erased
// signature of the bridge for `Function1.apply`, which would box the result.
// This test fails on 2.13.18 and on this branch: "bridge generated for member method apply ... clashes with
// definition of the member itself". With an explicit companion `object CB`, which is not a function, it compiles.
case class CA(a: Any) extends AnyVal
case class CR(a: AnyRef) extends AnyVal
object Newtype { type Type[A] }
case class CN(a: Newtype.Type[String]) extends AnyVal

case class CB(a: Any) extends AnyVal
object CB

object Test {
  def main(args: Array[String]): Unit = {
    println(CA(1))
    println(CR("r").a)
    println(CB(2))
  }
}
