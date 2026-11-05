package pwarn

class W12 {
  def proc() { println(12) }
  val sym = 'sym12
  val long = 12l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 12)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 12
  def use(w: W13) = w.old + w.proc().hashCode
}
