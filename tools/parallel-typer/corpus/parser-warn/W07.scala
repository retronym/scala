package pwarn

class W07 {
  def proc() { println(7) }
  val sym = 'sym7
  val long = 7l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 7)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 7
  def use(w: W08) = w.old + w.proc().hashCode
}
