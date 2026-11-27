trait N[-A]
trait Y extends N[Any]
object H { type NDY = N[D with Y] }
trait D extends N[H.NDY]

class Inv[A]
class Ev[-T](val s: String)
trait Low { implicit def any: Ev[(D, D)] = new Ev("fallback") }

// `D <: N[D with Y]` holds through the parent `Y` of `D with Y`, but the check tries the parent `D` first, which
// asks the same question again until it is cut off as a cycle. Every later `D <: H.NDY` in the same check has to
// get the answer of the first.
object Test extends Low {
  implicit def spec: Ev[(H.NDY, H.NDY)] = new Ev("specific")

  def one(d: D): H.NDY = d
  def two(p: (D, D)): (H.NDY, H.NDY) = p
  def nested(p: (D, List[D], List[List[D]])): (H.NDY, List[H.NDY], List[List[H.NDY]]) = p
  def contra(f: (H.NDY, H.NDY) => D): (D, D) => H.NDY = f
  def curried(f: H.NDY => H.NDY => D): D => D => H.NDY = f
  def bounds(p: (D, Inv[D])): (H.NDY, Inv[_ <: H.NDY]) = p
  def lower(p: (D, Inv[H.NDY])): (H.NDY, Inv[_ >: D]) = p
  def either(e: Either[D, D]): Either[H.NDY, H.NDY] = e
  def refined(p: (D, D) { def x: D }): (H.NDY, H.NDY) { def x: H.NDY } = p

  def f(p: (H.NDY, H.NDY)): String = "specific"
  def f(p: Any): String = "any"

  def poly[T](a: T, b: T)(implicit ev: (T, T) <:< (H.NDY, H.NDY)): String = "conforms"

  def main(args: Array[String]): Unit = {
    val d: D = new D {}
    val p: (D, D) = (d, d)
    assert(one(d) eq d)
    assert(two(p) eq p)
    assert(nested((d, Nil, Nil))._1 eq d)
    println(f(p))
    println(implicitly[Ev[(D, D)]].s)
    println(poly(d, d))
    println((p: Any) match { case _: (H.NDY, H.NDY) @unchecked => "matched" })
  }
}
