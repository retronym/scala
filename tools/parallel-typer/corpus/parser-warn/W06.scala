package pwarn

class W06 {
  def proc() { println(6) }
  val sym = 'sym6
  val long = 6l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 6)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 6
  def use(w: W07) = w.old + w.proc().hashCode
}
