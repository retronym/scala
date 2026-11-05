package pwarn

class W08 {
  def proc() { println(8) }
  val sym = 'sym8
  val long = 8l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 8)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 8
  def use(w: W09) = w.old + w.proc().hashCode
}
