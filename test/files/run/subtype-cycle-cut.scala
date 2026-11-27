trait N[-A]
trait Y extends N[Any]
object H { type NDY = N[D with Y] }
trait D extends N[H.NDY]

class Ev[-T](val s: String)
trait Low { implicit def any: Ev[(D, D)] = new Ev("fallback") }

// `D <: N[D with Y]` holds through the parent `Y` of `D with Y`, but the check tries the parent `D` first, which
// asks the same question again, until the recursion is cut off as a cycle. The cut-off pair must not be taken as
// known to be false: the second `D <: H.NDY` in each of the comparisons below has to get the answer of the first.
object Test extends Low {
  implicit def spec: Ev[(H.NDY, H.NDY)] = new Ev("specific")

  def one(d: D): H.NDY = d
  def two(p: (D, D)): (H.NDY, H.NDY) = p
  def nested(p: (D, List[D], List[List[D]])): (H.NDY, List[H.NDY], List[List[H.NDY]]) = p

  def f(p: (H.NDY, H.NDY)): String = "specific"
  def f(p: Any): String = "any"

  def main(args: Array[String]): Unit = {
    val d: D = new D {}
    val p: (D, D) = (d, d)
    assert(one(d) eq d)
    assert(two(p) eq p)
    assert(nested((d, Nil, Nil))._1 eq d)
    println(f(p))
    println(implicitly[Ev[(D, D)]].s)
  }
}
